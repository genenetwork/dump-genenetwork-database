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
  (schema-triples
   ;; TODO: Are ranges required for FOAF predicates? Can they not be
   ;; obtained from the FOAF spec?
   (foaf:name rdfs:range rdfs:Literal)
   (foaf:givenName rdfs:range rdfs:Literal)
   (foaf:familyName rdfs:range rdfs:Literal)
   (foaf:homepage rdfs:range rdfs:Literal)
   (gn-term:address rdfs:range rdfs:Literal)
   (gn-term:city rdfs:range rdfs:Literal)
   (gn-term:state rdfs:range rdfs:Literal)
   (gn-term:zipCode rdfs:range rdfs:Literal)
   (gn-term:country rdfs:range rdfs:Literal))
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
    (set gn-term:address (field Investigators Address))
    (set gn-term:city (field Investigators City))
    (set gn-term:state (field Investigators State))
    (set gn-term:zipCode (field Investigators ZipCode))
    (set gn-term:country (field Investigators Country))))

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
   (gn-term:dataset rdfs:range rdfs:Literal)
   (gn-term:datasetOfInvestigator rdfs:domain gn:dataset)
   (gn-term:datasetOfOrganization rdfs:domain gn:dataset)
   (gn-term:datasetOfInvestigator rdfs:range foaf:Person)
   (gn-term:datasetOfInbredSet rdfs:domain gn:dataset)
   (gn-term:datasetOfInbredSet rdfs:range gn:inbredSet)
   (gn-term:datasetOfSpecies rdfs:domain gn:dataset)
   (gn-term:datasetOfSpecies rdfs:range gn:inbredSet)
   (gn-term:datasetOfTissue rdfs:domain gn:dataset)
   (gn-term:datasetOfTissue rdfs:range gn:tissue)
   (gn-term:normalization rdfs:domain gn:dataset)
   (gn-term:normalization rdfs:range gn:avgMethod)
   (gn-term:datasetOfPlatform rdfs:domain gn:dataset)
   (gn-term:datasetOfPlatform rdfs:range gn:geneChip)
   (gn-term:accessionId rdfs:range rdfs:Literal)
   (gn-term:datasetStatusName rdfs:range rdfs:Literal)
   (gn-term:summary rdfs:range rdfs:Literal)
   (gn-term:aboutTissue rdfs:range rdfs:Literal)
   (gn-term:geoSeries rdfs:range rdfs:Literal)
   (gn-term:name rdfs:range rdfs:Literal)
   (gn-term:title rdfs:range rdfs:Literal)
   (gn-term:publicationTitle rdfs:range rdfs:Literal)
   (gn-term:specifics rdfs:range rdfs:Literal)
   (gn-term:datasetGroup rdfs:range rdfs:Literal)
   (gn-term:aboutCases rdfs:range rdfs:Literal)
   (gn-term:aboutPlatform rdfs:range rdfs:Literal)
   (gn-term:aboutDataProcessing rdfs:range rdfs:Literal)
   (gn-term:notes rdfs:range rdfs:Literal)
   (gn-term:experimentDesign rdfs:range rdfs:Literal)
   (gn-term:contributors rdfs:range rdfs:Literal)
   (gn-term:citation rdfs:range rdfs:Literal)
   (gn-term:acknowledgment rdfs:range rdfs:Literal))
  (triples (string->identifier
            "" (regexp-substitute/global #f "[^A-Za-z0-9:]"
                                        (field InfoFiles InfoPageName)
                                        'pre "_" 'post)
            #:separator ""
            #:proc string-capitalize-first)
    (set rdf:type (string->symbol
                   (field ("IF(GenoFreeze.Id IS NOT NULL, 'gn:genotypeDataset', IF(PublishFreeze.Id IS NOT NULL, 'gn:phenotypeDataset', 'gn:dataset'))"
                           rdfType))))
    (set gn-term:name (regexp-substitute/global
                       #f "^[Nn]one$"
                       (field InfoFiles InfoPageName)
                       ""))
    (set gn-term:fullName
         (field ("IFNULL(GenoFreeze.FullName, IFNULL(PublishFreeze.FullName, ''))"
                 DatasetFullName)))
    (set dct:created
         (field ("IFNULL(GenoFreeze.CreateTime, IFNULL(PublishFreeze.CreateTime, IFNULL(ProbeSetFreeze.CreateTime, '')))"
                 createTimeGenoFreeze)))
    (set gn-term:datasetOfInvestigator
         (investigator-attributes->id (field Investigators FirstName)
                                      (field Investigators LastName)
                                      (field Investigators Email)))
    (set gn-term:datasetOfOrganization
         (field ("CAST(CONVERT(BINARY CONVERT(Organizations.OrganizationName USING latin1) USING utf8) AS VARCHAR(1500))" Organizations)))
    (set gn-term:accessionId (format #f "GN~a" (field InfoFiles GN_AccesionId)))
    (set gn-term:datasetStatusName (string-downcase
                                    (field DatasetStatus DatasetStatusName)))
    (set gn-term:datasetOfInbredSet
         (string->identifier "inbredSet" (field InbredSet Name InbredSetName)))
    (set gn-term:datasetOfTissue (string->identifier "tissue"
                                                     (field Tissue Short_Name)))
    (set gn-term:normalization
         (string->identifier "avgmethod"
                             ;; If AvgMethodName is NULL, assume N/A.
                             (if (string-blank? (field AvgMethod Name AvgMethodName))
                                 "N/A" (field AvgMethod Name AvgMethodName))))
    (set gn-term:datasetOfPlatform
         (string->identifier "platform"
                             (field GeneChip Name GeneChip)))
    (set gn-term:summary
         (sanitize-rdf-string (field Datasets Summary)))
    (set gn-term:aboutTissue
         (sanitize-rdf-string (field Datasets AboutTissue)))
    (set gn-term:geoSeries
         (let ((s
                (string-match "GSE[0-9]*"
                              (field ("IFNULL(Datasets.GeoSeries, '')" GeoSeries)))))
           (if s (ontology
                  'geoSeries: (match:substring s))
               "")))
    (set gn-term:title
         (regexp-substitute/global
          #f "^[Nn]one$"
          (field InfoFiles InfoFileTitle)
          ""))
    (set gn-term:publicationTitle
         (regexp-substitute/global
          #f "^[Nn]one$"
          (field Datasets PublicationTitle)
          ""))
    (set gn-term:specifics (sanitize-rdf-string (field InfoFiles Specifics)))
    (set gn-term:datasetGroup (field Datasets DatasetName DatasetGroup))
    (set gn-term:aboutCases
         (sanitize-rdf-string
          (field ("CAST(CONVERT(BINARY CONVERT(Datasets.AboutCases USING latin1) USING utf8) AS VARCHAR(10000))" AboutCases))))
    (set gn-term:aboutPlatform
         (sanitize-rdf-string
          (field ("CAST(CONVERT(BINARY CONVERT(Datasets.AboutPlatform USING latin1) USING utf8) AS VARCHAR(1500))"
                  AboutPlatform))))
    (set gn-term:aboutDataProcessing
         (sanitize-rdf-string
          (field ("CAST(CONVERT(BINARY CONVERT(Datasets.AboutDataProcessing USING latin1) USING utf8) AS VARCHAR(1500))"
                  AboutDataProcessing))))
    (set gn-term:notes
         (sanitize-rdf-string
          (field ("CAST(CONVERT(BINARY CONVERT(Datasets.Notes USING latin1) USING utf8) AS VARCHAR(1500))"
                  GNNotes))))
    (set gn-term:experimentDesign
         (sanitize-rdf-string
          (field ("CAST(CONVERT(BINARY CONVERT(Datasets.ExperimentDesign USING latin1) USING utf8) AS VARCHAR(1500))"
                  ExperimentDesign))))
    (set gn-term:contributors
         (sanitize-rdf-string
          (field ("CAST(CONVERT(BINARY CONVERT(Datasets.Contributors USING latin1) USING utf8) AS VARCHAR(1500))"
                  Contributors))))
    (set gn-term:citation
         (sanitize-rdf-string
          (regexp-substitute/global
           #f "^[Nn]one$"
           (field
            ("CAST(CONVERT(BINARY CONVERT(Datasets.Citation USING latin1) USING utf8) AS VARCHAR(1500))"
             Citation))
           "")))
    (set gn-term:dataSourceAcknowledgment
         (sanitize-rdf-string
          (string-trim-both
           (regexp-substitute/global
            #f "^[Nn]one$"
            (field ("CAST(CONVERT(BINARY CONVERT(InfoFiles.Data_Source_Acknowledge USING latin1) USING utf8) AS VARCHAR(1500))"
                    Data_Source_Acknowledge))
            ""))))
    (set gn-term:acknowledgment (sanitize-rdf-string
                                 (field Datasets Acknowledgment)))))




(dump-with-documentation
 (name "Info files / Investigators Metadata")
 (connection %connection-settings)
 (table-metadata? #f)
 (prefixes
  '(("foaf:" "<http://xmlns.com/foaf/0.1/>")
    ("geoSeries:" "<http://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=>")
    ("gn-term:" "<http://genenetwork.org/term/>")
    ("gn:" "<http://genenetwork.org/id/>")
    ("rdf:" "<http://www.w3.org/1999/02/22-rdf-syntax-ns#>")
    ("rdfs:" "<http://www.w3.org/2000/01/rdf-schema#>")
    ("taxon:" "<http://purl.uniprot.org/taxonomy/>")
    ("dct:" "<http://purl.org/dc/terms/>")))
 (inputs
  (list dump-info-files
        dump-investigators))
 (outputs
  '(#:documentation "./docs/dump-info-pages.md"
    #:rdf "./verified-data/dump-info-pages.ttl")))

