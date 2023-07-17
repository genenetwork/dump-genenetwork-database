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
    (set gn:title (delete-substrings (field Publication Title)
                                     "Unknown"))
    (set gn:journal (delete-substrings (field Publication Journal)
                                       "Unknown"))
    (set gn:volume (delete-substrings (field Publication Volume)
                                      "Unknown"))
    (set gn:pages (delete-substrings (field Publication Pages)
                                     "Unknown"))
    (set gn:month (delete-substrings (field Publication Month)
                                     "Unknown"))
    (set gn:year (field Publication Year))
    (multiset gn:author
              ;; The authors field is a comma
              ;; separated list. Split it.
              (map string-trim (string-split (sanitize-rdf-string (field Publication Authors)) #\,)))
    (set gn:abstract
         (sanitize-rdf-string
          (field ("CAST(CONVERT(BINARY CONVERT(Publication.Abstract USING latin1) USING utf8) AS VARCHAR(100))" Abstract))))))



(dump-with-documentation
 (name "Publications Metadata")
 (connection %connection-settings)
 (table-metadata? #f)
 (prefixes
  (("rdf:" "<http://www.w3.org/1999/02/22-rdf-syntax-ns#>")
   ("rdfs:" "<http://www.w3.org/2000/01/rdf-schema#>")
   ("gn:" "<http://genenetwork.org/terms/>")
   ("publication:" "<http://genenetwork.org/publication/>")
   ("pubmed:" "<http://rdf.ncbi.nlm.nih.gov/pubmed/>")))
 (inputs
  (dump-publication))
 (outputs
  (#:documentation "./docs/dump-publication.md"
   #:rdf "./verified-data/dump-publication.md")))
