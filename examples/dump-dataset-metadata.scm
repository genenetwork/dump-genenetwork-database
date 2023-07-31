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
   (gnt:dataset rdfs:range rdfs:Literal)
   (gnt:datasetOfInvestigator rdfs:domain gn:dataset)
   (gnt:datasetOfOrganization rdfs:domain gn:dataset)
   (gnt:datasetOfInvestigator rdfs:range foaf:Person)
   (gnt:datasetOfInbredSet rdfs:domain gn:dataset)
   (gnt:datasetOfInbredSet rdfs:range gn:inbredSet)
   (gnt:datasetOfSpecies rdfs:domain gn:dataset)
   (gnt:datasetOfSpecies rdfs:range gn:inbredSet)
   (gnt:datasetOfTissue rdfs:domain gn:dataset)
   (gnt:datasetOfTissue rdfs:range gn:tissue)
   (gnt:normalization rdfs:domain gn:dataset)
   (gnt:normalization rdfs:range gn:avgMethod)
   (gnt:datasetOfPlatform rdfs:domain gn:dataset)
   (gnt:datasetOfPlatform rdfs:range gn:geneChip)
   (gnt:accessionId rdfs:range rdfs:Literal)
   (gnt:datasetStatusName rdfs:range rdfs:Literal)
   (gnt:summary rdfs:range rdfs:Literal)
   (gnt:aboutTissue rdfs:range rdfs:Literal)
   (gnt:geoSeries rdfs:range rdfs:Literal)
   (gnt:name rdfs:range rdfs:Literal)
   (gnt:title rdfs:range rdfs:Literal)
   (gnt:publicationTitle rdfs:range rdfs:Literal)
   (gnt:specifics rdfs:range rdfs:Literal)
   (gnt:datasetGroup rdfs:range rdfs:Literal)
   (gnt:aboutCases rdfs:range rdfs:Literal)
   (gnt:aboutPlatform rdfs:range rdfs:Literal)
   (gnt:aboutDataProcessing rdfs:range rdfs:Literal)
   (gnt:notes rdfs:range rdfs:Literal)
   (gnt:experimentDesign rdfs:range rdfs:Literal)
   (gnt:contributors rdfs:range rdfs:Literal)
   (gnt:citation rdfs:range rdfs:Literal)
   (gnt:acknowledgment rdfs:range rdfs:Literal))
  (triples (string->identifier
            "" (regexp-substitute/global #f "[^A-Za-z0-9:]"
                                        (field InfoFiles InfoPageName)
                                        'pre "_" 'post)
            #:separator ""
            #:proc string-capitalize-first)
    (set rdf:type (string->symbol
                   (field ("IF(GenoFreeze.Id IS NOT NULL, 'gn:genotypeDataset', IF(PublishFreeze.Id IS NOT NULL, 'gn:phenotypeDataset', 'gn:dataset'))"
                           rdfType))))
    (set gnt:name (regexp-substitute/global
                       #f "^[Nn]one$"
                       (field InfoFiles InfoPageName)
                       ""))
    (set gnt:fullName
         (field ("IFNULL(GenoFreeze.FullName, IFNULL(PublishFreeze.FullName, ''))"
                 DatasetFullName)))
    (set dct:created
         (field ("IFNULL(GenoFreeze.CreateTime, IFNULL(PublishFreeze.CreateTime, IFNULL(ProbeSetFreeze.CreateTime, '')))"
                 createTimeGenoFreeze)))
    (set gnt:datasetOfInvestigator
         (investigator-attributes->id (field Investigators FirstName)
                                      (field Investigators LastName)
                                      (field Investigators Email)))
    (set gnt:datasetOfOrganization
         (field ("CAST(CONVERT(BINARY CONVERT(Organizations.OrganizationName USING latin1) USING utf8) AS VARCHAR(1500))" Organizations)))
    (set gnt:accessionId (format #f "GN~a" (field InfoFiles GN_AccesionId)))
    (set gnt:datasetStatusName (string-downcase
                                    (field DatasetStatus DatasetStatusName)))
    (set gnt:datasetOfInbredSet
         (string->identifier "inbredSet" (field InbredSet Name InbredSetName)))
    (set gnt:datasetOfTissue (string->identifier "tissue"
                                                     (field Tissue Short_Name)))
    (set gnt:normalization
         (string->identifier "avgmethod"
                             ;; If AvgMethodName is NULL, assume N/A.
                             (if (string-blank? (field AvgMethod Name AvgMethodName))
                                 "N/A" (field AvgMethod Name AvgMethodName))))
    (set gnt:datasetOfPlatform
         (string->identifier "platform"
                             (field GeneChip Name GeneChip)))
    (set gnt:summary
         (sanitize-rdf-string (field Datasets Summary)))
    (set gnt:aboutTissue
         (sanitize-rdf-string (field Datasets AboutTissue)))
    (set gnt:geoSeries
         (let ((s
                (string-match "GSE[0-9]*"
                              (field ("IFNULL(Datasets.GeoSeries, '')" GeoSeries)))))
           (if s (ontology
                  'geoSeries: (match:substring s))
               "")))
    (set gnt:title
         (regexp-substitute/global
          #f "^[Nn]one$"
          (field InfoFiles InfoFileTitle)
          ""))
    (set gnt:publicationTitle
         (regexp-substitute/global
          #f "^[Nn]one$"
          (field Datasets PublicationTitle)
          ""))
    (set gnt:specifics (sanitize-rdf-string (field InfoFiles Specifics)))
    (set gnt:datasetGroup (field Datasets DatasetName DatasetGroup))
    (set gnt:aboutCases
         (sanitize-rdf-string
          (field ("CAST(CONVERT(BINARY CONVERT(Datasets.AboutCases USING latin1) USING utf8) AS VARCHAR(10000))" AboutCases))))
    (set gnt:aboutPlatform
         (sanitize-rdf-string
          (field ("CAST(CONVERT(BINARY CONVERT(Datasets.AboutPlatform USING latin1) USING utf8) AS VARCHAR(1500))"
                  AboutPlatform))))
    (set gnt:aboutDataProcessing
         (sanitize-rdf-string
          (field ("CAST(CONVERT(BINARY CONVERT(Datasets.AboutDataProcessing USING latin1) USING utf8) AS VARCHAR(1500))"
                  AboutDataProcessing))))
    (set gnt:notes
         (sanitize-rdf-string
          (field ("CAST(CONVERT(BINARY CONVERT(Datasets.Notes USING latin1) USING utf8) AS VARCHAR(1500))"
                  GNNotes))))
    (set gnt:experimentDesign
         (sanitize-rdf-string
          (field ("CAST(CONVERT(BINARY CONVERT(Datasets.ExperimentDesign USING latin1) USING utf8) AS VARCHAR(1500))"
                  ExperimentDesign))))
    (set gnt:contributors
         (sanitize-rdf-string
          (field ("CAST(CONVERT(BINARY CONVERT(Datasets.Contributors USING latin1) USING utf8) AS VARCHAR(1500))"
                  Contributors))))
    (set gnt:citation
         (sanitize-rdf-string
          (regexp-substitute/global
           #f "^[Nn]one$"
           (field
            ("CAST(CONVERT(BINARY CONVERT(Datasets.Citation USING latin1) USING utf8) AS VARCHAR(1500))"
             Citation))
           "")))
    (set gnt:dataSourceAcknowledgment
         (sanitize-rdf-string
          (string-trim-both
           (regexp-substitute/global
            #f "^[Nn]one$"
            (field ("CAST(CONVERT(BINARY CONVERT(InfoFiles.Data_Source_Acknowledge USING latin1) USING utf8) AS VARCHAR(1500))"
                    Data_Source_Acknowledge))
            ""))))
    (set gnt:acknowledgment (sanitize-rdf-string
                                 (field Datasets Acknowledgment)))))




(dump-with-documentation
 (name "Info files / Investigators Metadata")
 (connection %connection-settings)
 (table-metadata? #f)
 (prefixes
  '(("v:" "<http://www.w3.org/2006/vcard/ns#>")
    ("foaf:" "<http://xmlns.com/foaf/0.1/>")
    ("geoSeries:" "<http://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=>")
    ("gnt:" "<http://genenetwork.org/term/>")
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

