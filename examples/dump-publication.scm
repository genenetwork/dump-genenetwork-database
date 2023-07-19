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
   (gn-term:pubMedId rdfs:range rdfs:Literal)
   (gn-term:title rdfs:range rdfs:Literal)
   (gn-term:journal rdfs:range rdfs:Literal)
   (gn-term:volume rdfs:range rdfs:Literal)
   (gn-term:pages rdfs:range rdfs:Literal)
   (gn-term:month rdfs:range rdfs:Literal)
   (gn-term:year rdfs:range rdfs:Literal)
   (gn-term:author rdfs:range rdfs:Literal)
   (gn-term:abstract rdfs:range rdfs:Literal))
  (triples
      (let ((pmid (field
                   ("IF(Publication.PubMed_ID IS NULL, '', CONVERT(Publication.PubMed_Id, INT))"
                    pmid)))
            (publication-id (field Publication Id)))
        (if (string-null? pmid)
            (string->identifier "unpublished"
                                (number->string publication-id))
            (ontology 'pubmed: pmid)))
    (set rdf:type 'gn:publication)
    (set gn-term:pubMedId
         (ontology 'pubmed: (field ("IFNULL(PubMed_ID, '')" pubmedId))))
    (set gn-term:title (delete-substrings (field Publication Title)
                                          "Unknown"))
    (set gn-term:journal (delete-substrings (field Publication Journal)
                                            "Unknown"))
    (set gn-term:volume (delete-substrings (field Publication Volume)
                                           "Unknown"))
    (set gn-term:pages (delete-substrings (field Publication Pages)
                                          "Unknown"))
    (set gn-term:month (delete-substrings (field Publication Month)
                                          "Unknown"))
    (set gn-term:year (field Publication Year))
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
  '(("gn-term:" "<http://genenetwork.org/terms/>")
    ("gn:" "<http://genenetwork.org/id/>")
    ("pubmed:" "<http://rdf.ncbi.nlm.nih.gov/pubmed/>")
    ("rdfs:" "<http://www.w3.org/2000/01/rdf-schema#>")
    ("rdf:" "<http://www.w3.org/1999/02/22-rdf-syntax-ns#>")))
 (inputs
  (list dump-publication))
 (outputs
  '(#:documentation "./docs/dump-publication.md"
    #:rdf "./verified-data/dump-publication.md")))
