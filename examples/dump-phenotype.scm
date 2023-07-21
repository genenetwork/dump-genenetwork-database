#! /usr/bin/env guile
!#

(use-modules (rnrs programs)
             (rnrs io ports)
             (srfi srfi-1)
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



;; Only dump publish freeze entries that were not dumped from the InfoFiles page
(define-dump dump-publishfreeze
  (tables (PublishFreeze
           (left-join InfoFiles "ON InfoFiles.InfoPageName = PublishFreeze.Name")
           (left-join InbredSet "ON PublishFreeze.InbredSetId = InbredSet.InbredSetId"))
          "WHERE PublishFreeze.public > 0 AND PublishFreeze.confidentiality < 1 AND InfoFiles.InfoPageName IS NULL")
  (schema-triples
   (gn-term:datasetOfInbredSet rdfs:range gn:inbredSet)
   (gn-term:name rdfs:range rdfs:Literal)
   (gn-term:fullName rdfs:range rdfs:Literal)
   (gn-term:shortName rdfs:range rdfs:Literal)
   (gn:phenotypeDataset rdf:subClassOf gn:dataset))
  (triples
      (string->identifier
       ""
       (regexp-substitute/global #f "[^A-Za-z0-9:]"
                                 (field PublishFreeze Name)
                                 'pre "_" 'post)
       #:separator ""
       #:proc string-capitalize-first)
    (set rdf:type 'gn:phenotypeDataset)
    (set gn-term:name (field PublishFreeze Name))
    (set gn-term:fullName (field PublishFreeze FullName))
    (set gn-term:shortName (field PublishFreeze ShortName))
    (set dct:created (annotate-field
                      (field PublishFreeze CreateTime)
                      '^^xsd:date))
    (set gn-term:datasetOfInbredSet
         (string->identifier "inbredSet" (field InbredSet Name InbredSetName)))))

(define-dump dump-phenotypes
  (tables (Phenotype
           (left-join PublishXRef "ON Phenotype.Id = PublishXRef.PhenotypeId")
           (left-join Publication "ON Publication.Id = PublishXRef.PublicationId")
           (left-join PublishFreeze "ON PublishFreeze.InbredSetId = PublishXRef.InbredSetId")
           (left-join InfoFiles "ON InfoFiles.InfoPageName = PublishFreeze.Name")))
  (schema-triples
   (gn:phenotypeDataset rdfs:subPropertyOf gn:dataset)
   (gn-term:publicationDescription rdfs:range rdfs:Literal)
   (gn-term:originalDescription rdfs:range rdfs:Literal)
   (gn-term:prePublicationDescription rdfs:range rdfs:Literal)
   (gn-term:postPublicationAbbreviation rdfs:range rdfs:Literal)
   (gn-term:labCode rdfs:range rdfs:Literal)
   (gn-term:submitter rdfs:range rdfs:Literal)
   (gn-term:owner rdfs:range rdfs:Literal)
   (gn-term:mean rdfs:range xsd:double)
   (gn-term:LRS rdfs:range xsd:float)
   (gn-term:locus rdfs:range rdfs:Literal)
   (gn-term:additive rdfs:range xsd:decimal)
   (gn-term:sequence rdfs:range rdfs:Literal)
   (gn-term:phenotypeOfPublication rdfs:range gn-term:pubMedId))
  (triples (string->identifier
            ""
            (regexp-substitute/global #f "[^A-Za-z0-9:]"
                                        (field ("CONCAT(IF(PublishFreeze.Name IS NULL, '', CONCAT(PublishFreeze.Name, '_')), IF(Phenotype.Post_publication_abbreviation IS NULL, IF(Phenotype.Pre_publication_abbreviation IS NULL, Phenotype.Id, Pre_publication_abbreviation), Phenotype.Post_publication_abbreviation))" abbrev))
                                        'pre "_" 'post)
            #:separator ""
            #:proc string-capitalize-first)
    (set rdf:type 'gn:phenotype)
    (set gn-term:name (sanitize-rdf-string
                  (field
                   ("CAST(CONVERT(BINARY CONVERT(IF(Phenotype.Post_publication_abbreviation IS NULL, IF(Phenotype.Pre_publication_abbreviation IS NULL, Phenotype.Id, Phenotype.Pre_publication_abbreviation), Phenotype.Post_publication_abbreviation) USING latin1) USING utf8) AS VARCHAR(100))"
                    PhenotypeName))))
    ;; There is no row with an empty post-publication description so
    ;; use this field as the main publication description
    (set gn-term:publicationDescription
         (sanitize-rdf-string
          (field ("CAST(CONVERT(BINARY CONVERT(Phenotype.Post_publication_description USING latin1) USING utf8) AS CHAR(10000))"
                  postPubDescr))))
    (set gn-term:originalDescription (sanitize-rdf-string
                                 (delete-substrings
                                  (field Phenotype Original_description)
                                  "Original post publication description: ")))
    (set gn-term:prePublicationDescription
         (sanitize-rdf-string
          (field
           ("CAST(CONVERT(BINARY CONVERT(Phenotype.Pre_publication_description USING latin1) USING utf8) AS VARCHAR(15000))"
            prePubDesc))))
    (set gn-term:prePublicationAbbreviation (sanitize-rdf-string (field Phenotype Pre_publication_abbreviation)))
    (set gn-term:postPublicationAbbreviation (sanitize-rdf-string (field Phenotype Post_publication_abbreviation)))
    (set gn-term:labCode (field Phenotype Lab_code))
    (set gn-term:submitter (sanitize-rdf-string (field Phenotype Submitter)))
    (set gn-term:owner (sanitize-rdf-string (field Phenotype Owner)))
    (set gn-term:mean (annotate-field (field ("IFNULL(PublishXRef.mean, '')" mean))
                                 '^^xsd:double))
    (set gn-term:locus (field PublishXRef Locus))
    (set gn-term:LRS (annotate-field (field ("IFNULL(PublishXRef.LRS, '')" lrs)) '^^xsd:float))
    (set gn-term:additive (annotate-field (field ("IFNULL(PublishXRef.additive, '')" additive)) '^^xsd:decimal))
    (set gn-term:sequence (annotate-field (field PublishXRef Sequence) '^^xsd:int))
    (set gn-term:phenotypeOfDataset
         (string->identifier
          ""
          (field
           ("IFNULL(InfoFiles.InfoPageName, IFNULL(PublishFreeze.Name, ''))" DatasetName))
          #:separator ""
          #:proc string-capitalize-first))
    (set gn-term:phenotypeOfPublication
         (let ((pmid (field
                      ("IF(Publication.PubMed_ID IS NULL, '', CONVERT(Publication.PubMed_Id, INT))"
                       pmid)))
               (publication-id (field Publication Id)))
           (if (string-null? pmid)
               (string->identifier "unpublished"
                                   (number->string publication-id))
               (ontology 'pubmed: pmid))))))


(dump-with-documentation
 (name "Phenotypes Metadata")
 (connection %connection-settings)
 (table-metadata? #f)
 (prefixes
  '(("dct:" "<http://purl.org/dc/terms/>")
    ("gn:" "<http://genenetwork.org/id/>")
    ("gn-term:" "<http://genenetwork.org/terms/>")
    ("rdf:" "<http://www.w3.org/1999/02/22-rdf-syntax-ns#>")
    ("rdfs:" "<http://www.w3.org/2000/01/rdf-schema#>")
    ("xsd:" "<http://www.w3.org/2001/XMLSchema#>")
    ("pubmed:" "<http://rdf.ncbi.nlm.nih.gov/pubmed/>")))
 (inputs
  (list dump-publishfreeze
        dump-phenotypes))
 (outputs
  '(#:documentation "./docs/dump-phenotype.md"
    #:rdf "./verified-data/dump-phenotype.ttl")))
