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

(define %dump-directory
  (list-ref (command-line) 2))



;; Only dump publish freeze entries that were not dumped from the InfoFiles page
(define-dump dump-publishfreeze
  (tables (PublishFreeze
           (left-join InfoFiles "ON InfoFiles.InfoPageName = PublishFreeze.Name")
           (left-join InbredSet "ON PublishFreeze.InbredSetId = InbredSet.InbredSetId"))
          "WHERE PublishFreeze.public > 0 AND PublishFreeze.confidentiality < 1 AND InfoFiles.InfoPageName IS NULL")
  (schema-triples
   (gn:datasetOfInbredSet rdfs:range gn:inbredSet)
   (gn:name rdfs:range rdfs:Literal)
   (gn:fullName rdfs:range rdfs:Literal)
   (gn:shortName rdfs:range rdfs:Literal)
   (gn:createTime rdfs:range rdfs:Literal)
   (gn:phenotypeDataset rdf:subClassOf gn:dataset))
  (triples
      (ontology 'dataset:
                (regexp-substitute/global #f "[^A-Za-z0-9:]"
                                          (field PublishFreeze Name)
                                          'pre "_" 'post))
    (set rdf:type 'gn:phenotypeDataset)
    (set gn:name (field PublishFreeze Name))
    (set gn:fullName (field PublishFreeze FullName))
    (set gn:shortName (field PublishFreeze ShortName))
    (set dct:created (annotate-field
                      (field PublishFreeze CreateTime)
                      '^^xsd:date))
    (set gn:datasetOfInbredSet
         (string->identifier "inbredSet" (field InbredSet Name InbredSetName)))))

(define-dump dump-phenotypes
  (tables (Phenotype
           (left-join PublishXRef "ON Phenotype.Id = PublishXRef.PhenotypeId")
           (left-join Publication "ON Publication.Id = PublishXRef.PublicationId")
           (left-join PublishFreeze "ON PublishFreeze.InbredSetId = PublishXRef.InbredSetId")
           (left-join InfoFiles "ON InfoFiles.InfoPageName = PublishFreeze.Name")))
  (schema-triples
   (gn:phenotypeDataset rdfs:subPropertyOf gn:dataset))
  (triples (ontology 'phenotype:
                     (regexp-substitute/global #f "[^A-Za-z0-9:]"
                                               (field ("CONCAT(IF(PublishFreeze.Name IS NULL, '', CONCAT(PublishFreeze.Name, ':')), IF(Phenotype.Post_publication_abbreviation IS NULL, IF(Phenotype.Pre_publication_abbreviation IS NULL, Phenotype.Id, Pre_publication_abbreviation), Phenotype.Post_publication_abbreviation))" abbrev))
                                               'pre "_" 'post))
    (set rdf:type 'gn:phenotype)
    (set gn:name (sanitize-rdf-string
                  (field
                   ("CAST(CONVERT(BINARY CONVERT(IF(Phenotype.Post_publication_abbreviation IS NULL, IF(Phenotype.Pre_publication_abbreviation IS NULL, Phenotype.Id, Phenotype.Pre_publication_abbreviation), Phenotype.Post_publication_abbreviation) USING latin1) USING utf8) AS VARCHAR(100))"
                    PhenotypeName))))
    ;; There is no row with an empty post-publication description so
    ;; use this field as the main publication description
    (set gn:publicationDescription
         (sanitize-rdf-string
          (field ("CAST(CONVERT(BINARY CONVERT(Phenotype.Post_publication_description USING latin1) USING utf8) AS CHAR(10000))"
                  postPubDescr))))
    (set gn:originalDescription (sanitize-rdf-string
                                 (delete-substrings
                                  (field Phenotype Original_description)
                                  "Original post publication description: ")))
    (set gn:prePublicationDescription
         (sanitize-rdf-string
          (field
           ("CAST(CONVERT(BINARY CONVERT(Phenotype.Pre_publication_description USING latin1) USING utf8) AS VARCHAR(15000))"
            prePubDesc))))
    (set gn:prePublicationAbbreviation (sanitize-rdf-string (field Phenotype Pre_publication_abbreviation)))
    (set gn:postPublicationAbbreviation (sanitize-rdf-string (field Phenotype Post_publication_abbreviation)))
    (set gn:labCode (field Phenotype Lab_code))
    (set gn:submitter (sanitize-rdf-string (field Phenotype Submitter)))
    (set gn:owner (sanitize-rdf-string (field Phenotype Owner)))
    (set gn:mean (annotate-field (field ("IFNULL(PublishXRef.mean, '')" mean))
                                 '^^xsd:double))
    (set gn:locus (field PublishXRef Locus))
    (set gn:LRS (annotate-field (field ("IFNULL(PublishXRef.LRS, '')" lrs)) '^^xsd:float))
    (set gn:additive (annotate-field (field ("IFNULL(PublishXRef.additive, '')" additive)) '^^xsd:decimal))
    (set gn:sequence (annotate-field (field PublishXRef Sequence) '^^xsd:int))
    (set gn:phenotypeOfDataset
         (ontology 'dataset:
                   (regexp-substitute/global
                    #f "[^A-Za-z0-9:]"
                    (field ("IFNULL(InfoFiles.InfoPageName, IFNULL(PublishFreeze.Name, ''))" DatasetName))
                    'pre "_" 'post)))
    (set gn:phenotypeOfPublication
         (let ((pmid (field
                      ("IF(Publication.PubMed_ID IS NULL, '', CONVERT(Publication.PubMed_Id, INT))"
                       pmid)))
               (publication-id (field Publication Id)))
           (if (string-null? pmid)
               (string->identifier "unpublished"
                                   (number->string publication-id))
               (ontology 'publication: pmid))))))


(dump-with-documentation
 (name "Phenotypes Metadata")
 (connection %connection-settings)
 (table-metadata? #f)
 (prefixes
  '(("gn-id:" "<http://genenetwork.org/terms/>")
    ("gn-term:" "<http://genenetwork.org/terms/>")
    ("phenotype:" "<http://genenetwork.org/phenotype/>")
    ("rdf:" "<http://www.w3.org/1999/02/22-rdf-syntax-ns#>")
    ("rdfs:" "<http://www.w3.org/2000/01/rdf-schema#>")
    ("xsd:" "<http://www.w3.org/2001/XMLSchema#>")
    ("dataset:" "<http://genenetwork.org/dataset/>")
    ("publication:" "<http://genenetwork.org/publication/>")))
 (inputs
  (list dump-publishfreeze
        dump-phenotype))
 (outputs
  '(#:documentation "./docs/dump-phenotype.md"
    #:rdf "./verified-data/dump-phenotype.ttl")))
