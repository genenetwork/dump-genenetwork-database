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



;; These are phenotype datasets that don't have Infofile metadata
(define-dump dump-publishfreeze
  (tables (PublishFreeze
           (left-join InfoFiles "ON InfoFiles.InfoPageName = PublishFreeze.Name")
           (left-join InbredSet "ON PublishFreeze.InbredSetId = InbredSet.InbredSetId"))
          "WHERE PublishFreeze.public > 0 AND PublishFreeze.confidentiality < 1 AND InfoFiles.InfoFileId IS NULL")
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
           ;; We need this join so as to construct the trait's skos:altLabel
           (left-join InbredSet "ON InbredSet.InbredSetId = PublishXRef.InbredSetId")
           (left-join PublishFreeze "ON PublishFreeze.InbredSetId = PublishXRef.InbredSetId")
           (left-join InfoFiles "ON InfoFiles.InfoPageName = PublishFreeze.Name"))
          ;; Only dump public traits; Ignore "hanging" traits
          ;; I.e. traits that have no associated vectors
          "WHERE PublishFreeze.public > 0 AND PublishFreeze.confidentiality < 1 AND PublishFreeze.Id IS NOT NULL")
  (schema-triples
   (gnt:abbreviation a owl:ObjectProperty)
   (gnt:abbreviation rdfs:domain gnc:phenotype)
   (gnt:abbreviation skos:definition "The abbreviation used for this resource")
   (gnt:labCode a owl:ObjectProperty)
   (gnt:labCode rdfs:domain gnc:phenotype)
   (gnt:submitter a owl:ObjectProperty)
   (gnt:submitter rdfs:domain gnc:phenotype)
   (gnt:submitter skos:definition "A person who submitted this resource to GN")
   (gnt:mean rdfs:domain gnc:phenotype)
   (gnt:mean rdfs:range xsd:double)
   (gnt:LRS rdfs:domain gnc:phenotype)
   (gnt:LRS rdfs:range xsd:double)
   (gnt:locus rdfs:domain gnc:phenotype)
   (gnt:locus rdfs:range rdfs:Literal)
   (gnt:additive rdfs:domain gnc:phenotype)
   (gnt:additive rdfs:range xsd:double)
   (gnt:sequence rdfs:domain gnc:phenotype)
   (gnt:sequence rdfs:range xsd:integer))
  (triples (string->identifier
            ""
            (regexp-substitute/global #f "[^A-Za-z0-9:]"
                                      (field ("CONCAT(IF(PublishFreeze.Name IS NULL, '', CONCAT(PublishFreeze.Name, '_')), IF(Phenotype.Post_publication_abbreviation IS NULL, IF(Phenotype.Pre_publication_abbreviation IS NULL, Phenotype.Id, Pre_publication_abbreviation), Phenotype.Post_publication_abbreviation))" abbrev))
                                      'pre "_" 'post)
            #:separator ""
            #:proc string-capitalize-first)
    (set rdf:type 'gnc:phenotype)
    (set skos:prefLabel (sanitize-rdf-string
                         (field
                          ("IF(Phenotype.Post_publication_abbreviation IS NULL, IF(Phenotype.Pre_publication_abbreviation IS NULL, Phenotype.Id, Phenotype.Pre_publication_abbreviation), Phenotype.Post_publication_abbreviation)"
                           PhenotypeName))))
    ;; Add an alternative name for this resources.  This is how GN
    ;; currently labels phenotypes
    (set skos:altLabel (field
                        ("CONCAT(InbredSet.Name, '_', PublishXRef.Id)"
                         phenotypeAltName)))
    ;; All phenotypes have a post-publication description
    (set dct:description
         (sanitize-rdf-string
          (field Phenotype Post_publication_description)))
    ;; All phenotypes have a post-publication abbreviation
    (set gnt:abbreviation (field Phenotype Post_publication_abbreviation))
    (set gnt:labCode (field Phenotype Lab_code))
    (set gnt:submitter
         (sanitize-rdf-string (field Phenotype Submitter)))
    (set dct:contributor (sanitize-rdf-string (field Phenotype Owner)))
    (multiset dct:contributor (string-split
                               (sanitize-rdf-string (field Phenotype Owner))
                               #\,))
    (set gnt:mean (annotate-field (field ("IFNULL(PublishXRef.mean, '')" mean))
                                  '^^xsd:double))
    (set gnt:locus (field PublishXRef Locus))
    (set gnt:LRS (annotate-field
                  (field ("IFNULL(PublishXRef.LRS, '')" lrs))
                  '^^xsd:double))
    (set gnt:additive
         (annotate-field (field ("IFNULL(PublishXRef.additive, '')" additive))
                         '^^xsd:double))
    (set gnt:sequence (annotate-field (field PublishXRef Sequence) '^^xsd:integer))
    (set gnt:belongsToDataset
         (string->identifier
          ""
          (regexp-substitute/global #f "[^A-Za-z0-9:]"
                                    (field InfoFiles InfoPageName)
                                    'pre "_" 'post)
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
    ("owl:" "<http://www.w3.org/2002/07/owl#>")
    ("gnc:" "<http://genenetwork.org/category/>")
    ("gnt:" "<http://genenetwork.org/terms/>")
    ("skos:" "<http://www.w3.org/2004/02/skos/core#>")
    ("rdf:" "<http://www.w3.org/1999/02/22-rdf-syntax-ns#>")
    ("rdfs:" "<http://www.w3.org/2000/01/rdf-schema#>")
    ("xsd:" "<http://www.w3.org/2001/XMLSchema#>")
    ("pubmed:" "<http://rdf.ncbi.nlm.nih.gov/pubmed/>")))
 (inputs
  (list
   ;; dump-publishfreeze
   dump-phenotypes
   ))
 (outputs
  '(#:documentation "./docs/dump-phenotype.md"
    #:rdf "./verified-data/dump-phenotype.ttl")))
