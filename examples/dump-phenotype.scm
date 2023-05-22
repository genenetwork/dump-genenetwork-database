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
          "WHERE PublishFreeze.public > 0 AND InfoFiles.InfoPageName IS NULL")
  (schema-triples
   (gn:datasetOfInbredSet rdfs:range gn:inbredSet)
   (gn:name rdfs:range rdfs:Literal)
   (gn:fullName rdfs:range rdfs:Literal)
   (gn:shortName rdfs:range rdfs:Literal)
   (gn:createTime rdfs:range rdfs:Literal))
  (triples (string->identifier "dataset" (field PublishFreeze Name))
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
                   ("CAST(CONVERT(BINARY CONVERT(CONCAT(IF(PublishFreeze.Name IS NULL, '', CONCAT(PublishFreeze.Name, '-')), IF(Phenotype.Post_publication_abbreviation IS NULL, IF(Phenotype.Pre_publication_abbreviation IS NULL, Phenotype.Id, Phenotype.Pre_publication_abbreviation), Phenotype.Post_publication_abbreviation)) USING latin1) USING utf8) AS VARCHAR(10000))"
                    abbrev))))
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
                   (regexp-substitute/global #f "[^A-Za-z0-9:]"
                                             (field PublishFreeze Name)
                                             'pre "_" 'post)))
    (set gn:phenotypeOfPublication
         (let ((pmid (field
                      ("IF(Publication.PubMed_ID IS NULL, '', CONVERT(Publication.PubMed_Id, INT))"
                       pmid)))
               (publication-id (field Publication Id)))
           (if (string-null? pmid)
               (string->identifier "publication"
                                   (number->string publication-id))
               (ontology 'pubmed: pmid))))))


(call-with-target-database
 %connection-settings
 (lambda (db)
   (with-output-to-file (string-append %dump-directory "dump-phenotype.ttl")
     (lambda ()
       (prefix "chebi:" "<http://purl.obolibrary.org/obo/CHEBI_>")
       (prefix "dct:" "<http://purl.org/dc/terms/>")
       (prefix "foaf:" "<http://xmlns.com/foaf/0.1/>")
       (prefix "generif:" "<http://www.ncbi.nlm.nih.gov/gene?cmd=Retrieve&dopt=Graphics&list_uids=>")
       (prefix "gn:" "<http://genenetwork.org/>")
       (prefix "hgnc:" "<http://bio2rdf.org/hgnc:>")
       (prefix "homologene:" "<https://bio2rdf.org/homologene:>")
       (prefix "kegg:" "<http://bio2rdf.org/ns/kegg#>")
       (prefix "molecularTrait:" "<http://genenetwork.org/molecular-trait/>")
       (prefix "nuccore:" "<https://www.ncbi.nlm.nih.gov/nuccore/>")
       (prefix "omim:" "<https://www.omim.org/entry/>")
       (prefix "owl:" "<http://www.w3.org/2002/07/owl#>")
       (prefix "phenotype:" "<http://genenetwork.org/phenotype/>")
       (prefix "pubchem:" "<https://pubchem.ncbi.nlm.nih.gov/>")
       (prefix "pubmed:" "<http://rdf.ncbi.nlm.nih.gov/pubmed/>")
       (prefix "rdf:" "<http://www.w3.org/1999/02/22-rdf-syntax-ns#>")
       (prefix "rdfs:" "<http://www.w3.org/2000/01/rdf-schema#>")
       (prefix "taxon:" "<http://purl.uniprot.org/taxonomy/>")
       (prefix "uniprot:" "<http://purl.uniprot.org/uniprot/>")
       (prefix "up:" "<http://purl.uniprot.org/core/>")
       (prefix "xsd:" "<http://www.w3.org/2001/XMLSchema#>")
       (prefix "dataset:" "<http://genenetwork.org/dataset/>")
       (newline)
       (dump-publishfreeze db)
       (dump-phenotypes db))
     #:encoding "utf8")))
