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
   (gn:name rdfs:range rdfs:Literal))
  ;; Hopefully the Short_Name field is distinct and can be used as an
  ;; identifier.
  (triples (string->identifier "tissue" (field Tissue Short_Name))
	   (set rdf:type 'gn:tissue)
	   (set gn:name (field Tissue Name))))



(dump-with-documentation
 (name "Tissue Metadata")
 (connection %connection-settings)
 (table-metadata? #f)
 (prefixes
  (("rdf:" "<http://www.w3.org/1999/02/22-rdf-syntax-ns#>")
   ("rdfs:" "<http://www.w3.org/2000/01/rdf-schema#>")
   ("gn:" "<http://genenetwork.org/terms/>")))
 (inputs
  (dump-tissue))
 (outputs
  (#:documentation "./docs/dump-tissue.md" #:rdf "./verified-data/dump-tissue.ttl")))
