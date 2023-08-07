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
  (triples
      (string->identifier
       ""
       (regexp-substitute/global #f "[^A-Za-z0-9:]"
                                 (field PublishFreeze Name)
                                 'pre "_" 'post)
       #:separator ""
       #:proc string-capitalize-first)
    (set rdf:type 'gnc:phenotype)
    (set rdfs:label (field PublishFreeze Name))
    (set skos:prefLabel (field PublishFreeze FullName))
    (set skos:altLabel (field PublishFreeze ShortName))
    (set dct:created (annotate-field
                      (field PublishFreeze CreateTime)
                      '^^xsd:date))
    (set gnt:belongsToInbredSet
         (string->identifier
            "" (field InbredSet Name)
            #:separator ""
            #:proc string-capitalize-first))))

(define-dump dump-phenotypes
  (tables (Phenotype
           (left-join PublishXRef "ON Phenotype.Id = PublishXRef.PhenotypeId")
           (left-join Publication "ON Publication.Id = PublishXRef.PublicationId")
           (left-join PublishFreeze "ON PublishFreeze.InbredSetId = PublishXRef.InbredSetId")
           (left-join InfoFiles "ON InfoFiles.InfoPageName = PublishFreeze.Name")))
  (schema-triples
   (gnt:originalDescription a owl:ObjectProperty)
   (gnt:originalDescription rdfs:domain gnc:phenotype)
   (gnt:originalDescription skos:definition "The original description of this resource")
   (gnt:prePublicationDescription a owl:ObjectProperty)
   (gnt:prePublicationDescription rdfs:domain gnc:phenotype)
   (gnt:prePublicationDescription skos:definition "The pre publication details of this resource")
   (gnt:abbreviation a owl:ObjectProperty)
   (gnt:abbreviation rdfs:domain gnc:phenotype)
   (gnt:abbreviation skos:definition "The abbreviation used for this resource")
   (gnt:labCode rdfs:range rdfs:Literal)
   (gnt:submitter rdfs:range rdfs:Literal)
   (gnt:owner rdfs:range rdfs:Literal)
   (gnt:mean rdfs:range xsd:double)
   (gnt:LRS rdfs:range xsd:float)
   (gnt:locus rdfs:range rdfs:Literal)
   (gnt:additive rdfs:range xsd:decimal)
   (gnt:sequence rdfs:range rdfs:Literal)
   (gnt:phenotypeOfPublication rdfs:range gn-term:pubMedId))
  (triples (string->identifier
            ""
            (regexp-substitute/global #f "[^A-Za-z0-9:]"
                                      (field ("CONCAT(IF(PublishFreeze.Name IS NULL, '', CONCAT(PublishFreeze.Name, '_')), IF(Phenotype.Post_publication_abbreviation IS NULL, IF(Phenotype.Pre_publication_abbreviation IS NULL, Phenotype.Id, Pre_publication_abbreviation), Phenotype.Post_publication_abbreviation))" abbrev))
                                      'pre "_" 'post)
            #:separator ""
            #:proc string-capitalize-first)
    (set rdf:type 'gnc:phenotype)
    (set rdfs:label (sanitize-rdf-string
                     (field
                      ("IF(Phenotype.Post_publication_abbreviation IS NULL, IF(Phenotype.Pre_publication_abbreviation IS NULL, Phenotype.Id, Phenotype.Pre_publication_abbreviation), Phenotype.Post_publication_abbreviation)"
                       PhenotypeName))))
    ;; There is no row with an empty post-publication description so
    ;; use this field as the main publication description
    (set dct:description
         (sanitize-rdf-string
          (field Phenotype  Post_publication_description)))
    (set gnt:prePublicationAbbreviation (sanitize-rdf-string (field Phenotype Pre_publication_abbreviation)))
    (set gnt:postPublicationAbbreviation (sanitize-rdf-string (field Phenotype Post_publication_abbreviation)))
    (set gnt:labCode (field Phenotype Lab_code))
    (set gdmt:hasDistributorInfo
         (sanitize-rdf-string (field Phenotype Submitter)))
    (set gnt:owner (sanitize-rdf-string (field Phenotype Owner)))
    (set gnt:mean (annotate-field (field ("IFNULL(PublishXRef.mean, '')" mean))
                                  '^^xsd:double))
    (set gnt:locus (field PublishXRef Locus))
    (set gnt:LRS (annotate-field (field ("IFNULL(PublishXRef.LRS, '')" lrs)) '^^xsd:double))
    (set gnt:additive (annotate-field (field ("IFNULL(PublishXRef.additive, '')" additive)) '^^xsd:double))
    (set gnt:sequence (annotate-field (field PublishXRef Sequence) '^^xsd:int))
    (set gnt:belongsToDataset
         (string->identifier
          ""
          (field
           ("IFNULL(InfoFiles.InfoPageName, IFNULL(PublishFreeze.Name, ''))" DatasetName))
          #:separator ""
          #:proc string-capitalize-first))
    (set dct:isReferencedBy
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
    ("gnc:" "<http://genenetwork.org/category/>")
    ("gnt:" "<http://genenetwork.org/terms/>")
    ("skos:" "<http://www.w3.org/2004/02/skos/core#>")
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
