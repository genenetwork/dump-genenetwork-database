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


(define-dump dump-probeset
  (tables (ProbeSet
           (left-join GeneChip "ON GeneChip.Id = ProbeSet.ChipId")))
  (schema-triples
   (gn-term:name rdfs:range rdfs:Literal)
   (gn-term:probeset rdfs:range rdfs:Literal))
  (triples (ontology
            'probeset:
            (string-trim-both
             (regexp-substitute/global
              #f "[^A-Za-z0-9:]"
              (field ("IFNULL(NULLIF(TRIM(ProbeSet.Name), ''), ProbeSet.Id)"
                      name))
              'pre "_" 'post)))
    (set rdf:type 'gn-id:probeset)
    (set gn-term:chipOf (string->identifier "platform" (field GeneChip Name)))
    (set gn-term:name (field ProbeSet Name))
    (set gn-term:symbol (delete-substrings (field ProbeSet Symbol) "\""))
    (set gn-term:description (sanitize-rdf-string
                              (field ProbeSet description)))
    (set gn-term:chr (field ProbeSet Chr))
    (set gn-term:mb (annotate-field (field ("IFNULL(ProbeSet.Mb, '')" Mb)) '^^xsd:double))
    (set gn-term:blatSeq (sanitize-rdf-string
                          (string-trim-both (field ProbeSet BlatSeq))))
    (set gn-term:targetSeq (sanitize-rdf-string (field ProbeSet TargetSeq)))
    (set gn-term:uniProtReference (ontology 'uniprot:
                                            (field ProbeSet UniProtID)))))




(dump-with-documentation
 (name "ProbeSet Metadata")
 (connection %connection-settings)
 (table-metadata? #f)
 (prefixes
  '(("probeset:" "<http://genenetwork.org/probeset/>")
    ("rdf:" "<http://www.w3.org/1999/02/22-rdf-syntax-ns#>")
    ("rdfs:" "<http://www.w3.org/2000/01/rdf-r")
    ("uniprot:" "<http://purl.uniprot.org/uniprot/>")))
 (inputs
  (list dump-probeset))
 (outputs
  '(#:documentation "./docs/dump-probeset.md"
    #:rdf "./verified-data/dump-probeset.ttl")))
