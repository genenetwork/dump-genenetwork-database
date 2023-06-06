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

(define %dump-directory
  (list-ref (command-line) 2))



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
   (gn:address rdfs:range rdfs:Literal)
   (gn:city rdfs:range rdfs:Literal)
   (gn:state rdfs:range rdfs:Literal)
   (gn:zipCode rdfs:range rdfs:Literal)
   (gn:country rdfs:range rdfs:Literal))
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
    (set gn:address (field Investigators Address))
    (set gn:city (field Investigators City))
    (set gn:state (field Investigators State))
    (set gn:zipCode (field Investigators ZipCode))
    (set gn:country (field Investigators Country))))

(define-dump dump-info-files
  (tables (InfoFiles
           (left-join PublishFreeze "ON InfoFiles.InfoPageName = PublishFreeze.Name")
           (left-join GenoFreeze "ON InfoFiles.InfoPageName = GenoFreeze.Name")
           (left-join ProbeSetFreeze "ON InfoFiles.InfoPageName = ProbeSetFreeze.Name")
           (left-join InbredSet "ON InfoFiles.InbredSetId = InbredSet.InbredSetId")
           (left-join Datasets "USING (DatasetId)")
           (left-join DatasetStatus "USING (DatasetStatusId)")
           (left-join Tissue "USING (TissueId)")
           (left-join Investigators "USING (InvestigatorId)")
           (left-join AvgMethod "USING (AvgMethodId)")
           (left-join GeneChip "USING (GeneChipId)"))
          "WHERE GN_AccesionId IS NOT NULL AND ((PublishFreeze.public > 0 AND PublishFreeze.confidentiality < 1) OR (GenoFreeze.public > 0 AND GenoFreeze.confidentiality < 1) OR (ProbeSetFreeze.public > 0 AND ProbeSetFreeze.confidentiality < 1))")
  (schema-triples
   (gn:dataset rdfs:range rdfs:Literal)
   (gn:datasetOfInvestigator rdfs:domain gn:dataset)
   (gn:datasetOfOrganization rdfs:domain gn:dataset)
   (gn:datasetOfInvestigator rdfs:range foaf:Person)
   (gn:datasetOfInbredSet rdfs:domain gn:dataset)
   (gn:datasetOfInbredSet rdfs:range gn:inbredSet)
   (gn:datasetOfTissue rdfs:domain gn:dataset)
   (gn:datasetOfTissue rdfs:range gn:tissue)
   (gn:normalization rdfs:domain gn:dataset)
   (gn:normalization rdfs:range gn:avgMethod)
   (gn:datasetOfPlatform rdfs:domain gn:dataset)
   (gn:datasetOfPlatform rdfs:range gn:geneChip)
   (gn:accessionId rdfs:range rdfs:Literal)
   (gn:datasetStatusName rdfs:range rdfs:Literal)
   (gn:summary rdfs:range rdfs:Literal)
   (gn:aboutTissue rdfs:range rdfs:Literal)
   (gn:geoSeries rdfs:range rdfs:Literal)
   (gn:name rdfs:range rdfs:Literal)
   (gn:title rdfs:range rdfs:Literal)
   (gn:publicationTitle rdfs:range rdfs:Literal)
   (gn:specifics rdfs:range rdfs:Literal)
   (gn:datasetGroup rdfs:range rdfs:Literal)
   (gn:aboutCases rdfs:range rdfs:Literal)
   (gn:aboutPlatform rdfs:range rdfs:Literal)
   (gn:aboutDataProcessing rdfs:range rdfs:Literal)
   (gn:notes rdfs:range rdfs:Literal)
   (gn:experimentDesign rdfs:range rdfs:Literal)
   (gn:contributors rdfs:range rdfs:Literal)
   (gn:citation rdfs:range rdfs:Literal)
   (gn:acknowledgment rdfs:range rdfs:Literal))
  (triples (ontology 'dataset:
                     (regexp-substitute/global #f "[^A-Za-z0-9:]"
                                               (field InfoFiles InfoPageName)
                                               'pre "_" 'post))
    (set rdf:type (string->symbol
                   (field ("IF(GenoFreeze.Id IS NOT NULL, 'gn:genotypeDataset', IF(PublishFreeze.Id IS NOT NULL, 'gn:phenotypeDataset', 'gn:dataset'))"
                           rdfType))))
    (set gn:name (regexp-substitute/global
                  #f "^[Nn]one$"
                  (field InfoFiles InfoPageName)
                  ""))
    (set dct:created
         (field ("IFNULL(GenoFreeze.CreateTime, IFNULL(PublishFreeze.CreateTime, IFNULL(ProbeSetFreeze.CreateTime, '')))"
                 createTimeGenoFreeze)))
    (set gn:datasetOfInvestigator
         (investigator-attributes->id (field Investigators FirstName)
                                      (field Investigators LastName)
                                      (field Investigators Email)))
    (set gn:datasetOfOrganization
         (field ("CAST(CONVERT(BINARY CONVERT(Organizations.OrganizationName USING latin1) USING utf8) AS VARCHAR(1500))" Organizations)))
    (set gn:accessionId (string-append "GN" (number->string
                                             (field InfoFiles GN_AccesionId))))
    (set gn:datasetStatusName (string-downcase
                               (field DatasetStatus DatasetStatusName)))
    (set gn:datasetOfInbredSet
         (string->identifier "inbredSet" (field InbredSet Name InbredSetName)))
    (set gn:datasetOfTissue (string->identifier "tissue"
                                                (field Tissue Short_Name)))
    (set gn:normalization
         (string->identifier "avgmethod"
                             ;; If AvgMethodName is NULL, assume N/A.
                             (if (string-blank? (field AvgMethod Name AvgMethodName))
                                 "N/A" (field AvgMethod Name AvgMethodName))))
    (set gn:datasetOfPlatform
         (string->identifier "platform"
                             (field GeneChip Name GeneChip)))
    (set gn:summary
         (sanitize-rdf-string (field Datasets Summary)))
    (set gn:aboutTissue
         (sanitize-rdf-string (field Datasets AboutTissue)))
    (set gn:geoSeries
         (let ((s
                (string-match "GSE[0-9]*"
                              (field ("IFNULL(Datasets.GeoSeries, '')" GeoSeries)))))
           (if s (ontology
                  'geoSeries: (match:substring s))
               "")))
    (set gn:title
         (regexp-substitute/global
          #f "^[Nn]one$"
          (field InfoFiles Title)
    (set gn:publicationTitle
         (regexp-substitute/global
          #f "^[Nn]one$"
          (field Datasets PublicationTitle)
          ""))
    (set gn:specifics (sanitize-rdf-string (field InfoFiles Specifics)))
    (set gn:datasetGroup (field Datasets DatasetName DatasetGroup))
    (set gn:aboutCases
         (sanitize-rdf-string
          (field ("CAST(CONVERT(BINARY CONVERT(Datasets.AboutCases USING latin1) USING utf8) AS VARCHAR(10000))" AboutCases))))
    (set gn:aboutPlatform
         (sanitize-rdf-string
          (field ("CAST(CONVERT(BINARY CONVERT(Datasets.AboutPlatform USING latin1) USING utf8) AS VARCHAR(1500))"
                  AboutPlatform))))
    (set gn:aboutDataProcessing
         (sanitize-rdf-string
          (field ("CAST(CONVERT(BINARY CONVERT(Datasets.AboutProcessing USING latin1) USING utf8) AS VARCHAR(1500))"
                  AboutProcessing))))
    (set gn:notes
         (sanitize-rdf-string
          (field ("CAST(CONVERT(BINARY CONVERT(Datasets.Notes USING latin1) USING utf8) AS VARCHAR(1500))"
                  Notes))))
    (set gn:experimentDesign
         (sanitize-rdf-string
          (field ("CAST(CONVERT(BINARY CONVERT(Datasets.ExperimentDesign USING latin1) USING utf8) AS VARCHAR(1500))"
                  ExperimentDesign))))
    (set gn:contributors
         (sanitize-rdf-string
          (field ("CAST(CONVERT(BINARY CONVERT(Datasets.Contributors USING latin1) USING utf8) AS VARCHAR(1500))"
                  Contributors))))
    (set gn:citation
         (sanitize-rdf-string
          (regexp-substitute/global
           #f "^[Nn]one$"
           (field
            ("CAST(CONVERT(BINARY CONVERT(IFNULL(IF(InfoFiles.Citation = 'None' OR InfoFiles.Citation = '' OR InfoFiles.Citation IS NULL, Datasets.Citation, InfoFiles.Citation), '') USING latin1) USING utf8) AS VARCHAR(1500))"
             Citation))
           "")))
    (set gn:dataSourceAcknowledgment
         (sanitize-rdf-string
          (string-trim-both
           (regexp-substitute/global
            #f "^[Nn]one$"
            (field ("CAST(CONVERT(BINARY CONVERT(InfoFiles.Data_Source_Acknowledge USING latin1) USING utf8) AS VARCHAR(1500))"
                    Data_Source_Acknowledge))
            ""))))
    (set gn:acknowledgment (sanitize-rdf-string
                            (field Datasets Acknowledgment)))))




(call-with-target-database
 %connection-settings
 (lambda (db)
   (with-output-to-file (string-append %dump-directory "dump-info-pages.ttl")
     (lambda ()
       (prefix "dct:" "<http://purl.org/dc/terms/>")
       (prefix "foaf:" "<http://xmlns.com/foaf/0.1/>")
       (prefix "generif:" "<http://www.ncbi.nlm.nih.gov/gene?cmd=Retrieve&dopt=Graphics&list_uids=>")
       (prefix "geoSeries:" "<http://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=>")
       (prefix "gn:" "<http://genenetwork.org/>")
       (prefix "owl:" "<http://www.w3.org/2002/07/owl#>")
       (prefix "phenotype:" "<http://genenetwork.org/phenotype/>")
       (prefix "pubmed:" "<http://rdf.ncbi.nlm.nih.gov/pubmed/>")
       (prefix "rdf:" "<http://www.w3.org/1999/02/22-rdf-syntax-ns#>")
       (prefix "rdfs:" "<http://www.w3.org/2000/01/rdf-schema#>")
       (prefix "uniprot:" "<http://purl.uniprot.org/uniprot/>")
       (prefix "up:" "<http://purl.uniprot.org/core/>")
       (prefix "xsd:" "<http://www.w3.org/2001/XMLSchema#>")
       (prefix "probeset:" "<http://genenetwork.org/probeset/>")
       (prefix "dataset:" "<http://genenetwork.org/dataset/>")
       (newline)
       (dump-info-files db)
       (dump-investigators db))
     #:encoding "utf8")))
