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



(define-dump dump-publication
  (tables (Publication))
  (schema-triples
   (gn:pubMedId rdfs:range rdfs:Literal)
   (gn:title rdfs:range rdfs:Literal)
   (gn:journal rdfs:range rdfs:Literal)
   (gn:volume rdfs:range rdfs:Literal)
   (gn:pages rdfs:range rdfs:Literal)
   (gn:month rdfs:range rdfs:Literal)
   (gn:year rdfs:range rdfs:Literal)
   (gn:author rdfs:range rdfs:Literal)
   (gn:abstract rdfs:range rdfs:Literal))
  (triples
      (let ((pmid (field
                   ("IF(Publication.PubMed_ID IS NULL, '', CONVERT(Publication.PubMed_Id, INT))"
                    pmid)))
            (publication-id (field Publication Id)))
        (if (string-null? pmid)
            (string->identifier "unpublished"
                                (number->string publication-id))
            (ontology 'publication: pmid)))
    (set rdf:type 'gn:publication)
    (set gn:pubMedId (ontology 'pubmed: (field ("IFNULL(PubMed_ID, '')" pubmedId))))
    (set gn:title (field Publication Title))
    (set gn:journal (field Publication Journal))
    (set gn:volume (field Publication Volume))
    (set gn:pages (field Publication Pages))
    (set gn:month (field Publication Month))
    (set gn:year (field Publication Year))
    (multiset gn:author
              ;; The authors field is a comma
              ;; separated list. Split it.
              (map string-trim (string-split (sanitize-rdf-string (field Publication Authors)) #\,)))
    (set gn:abstract
         (sanitize-rdf-string
          (field ("CAST(CONVERT(BINARY CONVERT(Publication.Abstract USING latin1) USING utf8) AS VARCHAR(100))" Abstract))))))



(call-with-target-database
 %connection-settings
 (lambda (db)
   (with-output-to-file (string-append %dump-directory "dump-publication.ttl")
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
       (prefix "uniprot:" "<http://purl.uniprot.org/uniprot/>")
       (prefix "up:" "<http://purl.uniprot.org/core/>")
       (prefix "xsd:" "<http://www.w3.org/2001/XMLSchema#>")
       (prefix "publication:" "<http://genenetwork.org/publication/>")
       (newline)
       (dump-publication db))
     #:encoding "utf8")))
