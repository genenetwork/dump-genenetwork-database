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



(define-dump dump-tissue
    ;; The Name and TissueName fields seem to be identical. BIRN_lex_ID
    ;; and BIRN_lex_Name are mostly NULL.
    (tables (Tissue))
  (schema-triples
   (gnc:tissue a skos:Concept))
  ;; Hopefully the Short_Name field is distinct and can be used as an
  ;; identifier.
  (triples (string->identifier "tissue" (field Tissue Short_Name))
	   (set rdf:type 'gnc:tissue)
	   (set rdfs:label (field Tissue Name))))



(dump-with-documentation
 (name "Tissue Metadata")
 (connection %connection-settings)
 (table-metadata? #f)
 (prefixes
  '(("gn:" "<http://genenetwork.org/id/>")
    ("gnt:" "<http://genenetwork.org/terms/>")
    ("gnc:" "<http://genenetwork.org/category/>")
    ("rdf:" "<http://www.w3.org/1999/02/22-rdf-syntax-ns#>")
    ("rdfs:" "<http://www.w3.org/2000/01/rdf-schema#>")))
 (inputs
  (list dump-tissue))
 (outputs
  '(#:documentation "./docs/dump-tissue.md"
    #:rdf "./verified-data/dump-tissue.ttl")))
