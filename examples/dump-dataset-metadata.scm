#! /usr/bin/env guile
!#

(use-modules (srfi srfi-1)
             (srfi srfi-26)
             (ice-9 match)
             (ice-9 regex)
             (dump strings)
             (dump sql)
             (dump triples)
             (dump special-forms))



(define %connection-settings
  (call-with-input-file (list-ref (command-line) 1)
    read))



;; One email ID in the Investigators table has spaces in it. This
;; function fixes that.
(define (fix-email-id email)
  (string-delete #\space email))

(define (investigator-attributes->id first-name last-name email)
  ;; There is just one record corresponding to "Evan Williams" which
  ;; does not have an email ID. To accommodate that record, we
  ;; construct the investigator ID from not just the email ID, but
  ;; also the first and the last names. It would be preferable to just
  ;; find Evan Williams' email ID and insert it into the database.
  (string->identifier "investigator"
                      (string-join
                       ;; Add special case for Yohan Bossé whose name
                       ;; has unprintable characters.
                       ;; TODO: Fix Yohan Bossé's name in the database.
                       (let ((last-name (if (string=? first-name "Yohan")
                                            "Bosse"
                                            last-name)))
                         (list first-name last-name (fix-email-id email)))
                       "_")))

(define-dump dump-investigators
  ;; There are a few duplicate entries. We group by email to
  ;; deduplicate.
  (tables (Investigators)
          "GROUP BY Email")
  (triples (investigator-attributes->id (field Investigators FirstName)
                                        (field Investigators LastName)
                                        (field Investigators Email))
    (set rdf:type 'foaf:Person)
    (set foaf:name (string-append (field Investigators FirstName) " "
                                  (if (string=? (field Investigators FirstName) "Yohan")
                                      "Bossé"
                                      (field Investigators LastName))))
    (set foaf:givenName
         (field ("CAST(CONVERT(BINARY CONVERT(FirstName USING latin1) USING utf8) AS VARCHAR(100))" FirstName)))
    (set foaf:familyName
         (field ("CAST(CONVERT(BINARY CONVERT(LastName USING latin1) USING utf8) AS VARCHAR(100))" LastName)))
    (set foaf:homepage (field Investigators Url))
    (set v:adr (field Investigators Address))
    (set v:locality (field Investigators City))
    (set v:region (field Investigators State))
    (set v:postal-code (field Investigators ZipCode))
    (set v:country-name (field Investigators Country))))

(define-dump dump-info-files
  (tables (InfoFiles
           (left-join PublishFreeze "ON InfoFiles.InfoPageName = PublishFreeze.Name")
           (left-join GenoFreeze "ON InfoFiles.InfoPageName = GenoFreeze.Name")
           (left-join ProbeSetFreeze "ON InfoFiles.InfoPageName = ProbeSetFreeze.Name")
           (left-join InbredSet "ON InfoFiles.InbredSetId = InbredSet.InbredSetId")
           (left-join Species "ON InfoFiles.SpeciesId = Species.SpeciesId")
           (left-join Datasets "USING (DatasetId)")
           (left-join DatasetStatus "USING (DatasetStatusId)")
           (left-join Tissue "USING (TissueId)")
           (left-join Investigators "USING (InvestigatorId)")
           (left-join AvgMethod "USING (AvgMethodId)")
           (left-join Organizations "USING (OrganizationId)")
           (left-join GeneChip "USING (GeneChipId)"))
          "WHERE GN_AccesionId IS NOT NULL")
  (schema-triples
   (gnc:dataset rdf:type gdmt:Dataset)
   (gnc:genotype rdfs:subClassOf gnc:dataset)
   (gnc:phenotype rdfs:subClassOf gnc:dataset)
   (gnt:belongsToInbredSet rdfs:domain gnc:dataset)
   (gnt:belongsToInbredSet a owl:ObjectProperty)
   (gnt:belongsToInbredSet skos:definition "The InbredSet this resource belongs to")
   (gnt:hasTissue rdfs:domain gnc:dataset)
   (gnt:hasTissue a owl:ObjectProperty)
   (gnt:hasTissue skos:definition "Tissues this resource has")
   (gnt:hasTissueInfo rdfs:domain gnc:dataset)
   (gnt:hasTissueInfo a owl:ObjectProperty)
   (gnt:hasTissueInfo skos:definition "Metadata about Tissue for this resource")
   (gnt:usedNormalization rdfs:domain gnc:dataset)
   (gnt:usedNormalization a owl:ObjectProperty)
   (gnt:usedNormalization skos:definition "Normalization techniques this resource has")
   (gnt:usedPlatform rdfs:domain gnc:dataset)
   (gnt:usedPlatform a owl:ObjectProperty)
   (gnt:usedPlatform skos:definition "The Platform this resource uses")
   (gnt:hasGeoSeriesId rdfs:domain gnc:dataset)
   (gnt:hasGeoSeriesId a owl:ObjectProperty)
   (gnt:hasGeoSeriesId skos:definition "id of record in NCBI database")
   (gnt:hasExperimentDesignInfo rdfs:domain gnc:dataset)
   (gnt:hasExperimentDesignInfo rdfs:label "Experiment Design")
   (gnt:hasExperimentDesignInfo a owl:ObjectProperty)
   (gnt:hasExperimentDesignInfo skos:definition "Information about how the experiment was designed")
   (gnt:hasNotes rdfs:domain gnc:dataset)
   (gnt:hasNotes a owl:ObjectProperty)
   (gnt:hasNotes rdfs:label "Notes")
   (gnt:hasNotes skos:definition "Extra Notes about this dataset")
   (gnt:hasDataProcessingInfo rdfs:domain gnc:dataset)
   (gnt:hasDataProcessingInfo rdfs:label "About Data Processing")
   (gnt:hasDataProcessingInfo a owl:ObjectProperty)
   (gnt:hasDataProcessingInfo skos:definition "Information about how this dataset was processed")
   (gnt:hasPlatformInfo rdfs:domain gnc:dataset)
   (gnt:hasPlatformInfo a owl:ObjectProperty)
   (gnt:hasPlatformInfo rdfs:label "About Platfoorm")
   (gnt:hasPlatformInfo skos:definition "Information about the platform that was used with this dataset")
   (gnt:hasCaseInfo rdfs:domain gnc:dataset)
   (gnt:hasCaseInfo rdfs:label "About Case")
   (gnt:hasCaseInfo a owl:ObjectProperty)
   (gnt:hasCaseInfo skos:definition "Information about the cases used in this platform")
   (gnt:hasAcknowledgement rdfs:domain gnc:dataset)
   (gnt:hasAcknowledgement rdfs:label "Acknowledgement")
   (gnt:hasAcknowledgement a owl:ObjectProperty)
   (gnt:hasAcknowledgement skos:definition "People to acknowledge"))
  (triples (string->identifier
            "" (regexp-substitute/global #f "[^A-Za-z0-9:]"
                                         (field InfoFiles InfoPageName)
                                         'pre "_" 'post)
            #:separator ""
            #:proc string-capitalize-first)
    (set rdf:type (string->symbol
                   (field ("IF(GenoFreeze.Id IS NOT NULL, 'gnc:genotype', IF(PublishFreeze.Id IS NOT NULL, 'gnc:phenotype', 'gnc:dataset'))"
                           rdfType))))
    (set rdfs:label (regexp-substitute/global
                     #f "^[Nn]one$"
                     (field InfoFiles InfoPageName)
                     ""))
    (set gdmt:hasTitleInfo
         (field ("IFNULL(GenoFreeze.FullName, IFNULL(PublishFreeze.FullName, ''))"
                 DatasetFullName)))
    (set gdmt:hasTitleInfo (field Datasets DatasetName DatasetGroup))
    (set gdmt:hasTitleInfo
         (regexp-substitute/global
          #f "^[Nn]one$"
          (field InfoFiles InfoFileTitle)
          ""))
    ;; This is the published title
    (set dct:title
         (regexp-substitute/global
          #f "^[Nn]one$"
          (field Datasets PublicationTitle)
          ""))
    (set dct:created
         (field ("IFNULL(GenoFreeze.CreateTime, IFNULL(PublishFreeze.CreateTime, IFNULL(ProbeSetFreeze.CreateTime, '')))"
                 createTimeGenoFreeze)))
    (set gdmt:hasCreatorInfo
         (investigator-attributes->id (field Investigators FirstName)
                                      (field Investigators LastName)
                                      (field Investigators Email)))
    (set gdmt:hasCreatorAffiliation
         (field ("CAST(CONVERT(BINARY CONVERT(Organizations.OrganizationName USING latin1) USING utf8) AS VARCHAR(1500))" Organizations)))
    (set gdmt:hasDatasetIdentifierSubType (format #f "GN~a" (field InfoFiles GN_AccesionId)))
    (set gdmt:hasRightsInfo (string-downcase
                             (field DatasetStatus DatasetStatusName)))
    (set gnt:belongsToInbredSet
         (string->identifier "inbredSet" (field InbredSet Name InbredSetName)))
    (set gnt:hasTissue (string->identifier "tissue"
                                           (field Tissue Short_Name)))
    (set gnt:usedNormalization
         (string->identifier "avgmethod"
                             ;; If AvgMethodName is NULL, assume N/A.
                             (if (string-blank? (field AvgMethod Name AvgMethodName))
                                 "N/A" (field AvgMethod Name AvgMethodName))))
    (set gnt:usedPlatform
         (string->identifier "platform"
                             (field GeneChip Name GeneChip)))
    (set gdmt:isDescribedBy
         (sanitize-rdf-string (field Datasets Summary)))
    (set gnt:hasGeoSeriesId
         (let ((s
                (string-match "GSE[0-9]*"
                              (field ("IFNULL(Datasets.GeoSeries, '')" GeoSeries)))))
           (if s (ontology
                  'geoSeries: (match:substring s))
               "")))
    (set gnt:hasTissueInfo
         (sanitize-rdf-string (field Datasets AboutTissue)))
    (set gnt:hasContentInfo (sanitize-rdf-string (field InfoFiles Specifics)))
    (set gnt:hasCaseInfo
         (sanitize-rdf-string
          (field ("CAST(CONVERT(BINARY CONVERT(Datasets.AboutCases USING latin1) USING utf8) AS VARCHAR(10000))" AboutCases))))
    (set gnt:hasPlatformInfo
         (sanitize-rdf-string
          (field ("CAST(CONVERT(BINARY CONVERT(Datasets.AboutPlatform USING latin1) USING utf8) AS VARCHAR(1500))"
                  AboutPlatform))))
    (set gnt:hasDataProcessingInfo
         (sanitize-rdf-string
          (field ("CAST(CONVERT(BINARY CONVERT(Datasets.AboutDataProcessing USING latin1) USING utf8) AS VARCHAR(1500))"
                  AboutDataProcessing))))
    (set gnt:hasNotes
         (sanitize-rdf-string
          (field ("CAST(CONVERT(BINARY CONVERT(Datasets.Notes USING latin1) USING utf8) AS VARCHAR(1500))"
                  GNNotes))))
    (set gnt:hasExperimentDesignInfo
         (sanitize-rdf-string
          (field ("CAST(CONVERT(BINARY CONVERT(Datasets.ExperimentDesign USING latin1) USING utf8) AS VARCHAR(1500))"
                  ExperimentDesign))))
    (set gdmt:hasContributorInfo
         (sanitize-rdf-string
          (field ("CAST(CONVERT(BINARY CONVERT(Datasets.Contributors USING latin1) USING utf8) AS VARCHAR(1500))"
                  Contributors))))
    (set gdmt:IsCitedBy
         (sanitize-rdf-string
          (regexp-substitute/global
           #f "^[Nn]one$"
           (field
            ("CAST(CONVERT(BINARY CONVERT(Datasets.Citation USING latin1) USING utf8) AS VARCHAR(1500))"
             Citation))
           "")))
    (set gnt:hasAcknowledgement
         (sanitize-rdf-string
          (string-trim-both
           (regexp-substitute/global
            #f "^[Nn]one$"
            (field ("CAST(CONVERT(BINARY CONVERT(InfoFiles.Data_Source_Acknowledge USING latin1) USING utf8) AS VARCHAR(1500))"
                    Data_Source_Acknowledge))
            ""))))
    (set gnt:hasAcknowledgement (sanitize-rdf-string
                                 (field Datasets Acknowledgment)))))




(dump-with-documentation
 (name "Info files / Investigators Metadata")
 (connection %connection-settings)
 (table-metadata? #f)
 (prefixes
  '(("v:" "<http://www.w3.org/2006/vcard/ns#>")
    ("foaf:" "<http://xmlns.com/foaf/0.1/>")
    ("gdmt:" "<http://vocab.fairdatacollective.org/gdmt/>")
    ("skos:" "<http://www.w3.org/2004/02/skos/core#>")
    ("geoSeries:" "<http://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=>")
    ("gnt:" "<http://genenetwork.org/term/>")
    ("gn:" "<http://genenetwork.org/id/>")
    ("gnc:" "<http://genenetwork.org/category/>")
    ("rdf:" "<http://www.w3.org/1999/02/22-rdf-syntax-ns#>")
    ("owl:" "<http://www.w3.org/2002/07/owl#>")
    ("rdfs:" "<http://www.w3.org/2000/01/rdf-schema#>")
    ("taxon:" "<http://purl.uniprot.org/taxonomy/>")
    ("dct:" "<http://purl.org/dc/terms/>")))
 (inputs
  (list dump-info-files
        dump-investigators))
 (outputs
  '(#:documentation "./docs/dump-info-pages.md"
    #:rdf "./verified-data/dump-info-pages.ttl")))

