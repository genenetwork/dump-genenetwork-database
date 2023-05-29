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


(define-dump dump-probeset
  (tables (ProbeSet
           (left-join GeneChip "ON GeneChip.Id = ProbeSet.ChipId")))
  (schema-triples
   (gn:name rdfs:range rdfs:Literal)
   (gn:probeset rdfs:range rdfs:Literal))
  (triples (ontology
            'probeset:
            (regexp-substitute/global
             #f "[^A-Za-z0-9:]"
             (field ("IFNULL(ProbeSet.Name, ProbeSet.Id)"
                     name))
             'pre "_" 'post))
    (set rdf:type 'gn:probeset)
    (set gn:chipOf (string->identifier "platform" (field GeneChip Name)))
    (set gn:name (field ProbeSet Name))
    (set gn:symbol (field ProbeSet Symbol))
    (set gn:description (sanitize-rdf-string
                         (field ProbeSet description)))
    (set gn:chr (field ProbeSet Chr))
    (set gn:mb (annotate-field (field ("IFNULL(ProbeSet.Mb, '')" Mb)) '^^xsd:double))
    (multiset gn:tissue (map string-trim-both
                             (string-split
                              (field ("IFNULL(ProbeSet.Tissue, '')" Tissue))
                              #\,)))
    (multiset gn:alias (map string-trim-both
                            (string-split (sanitize-rdf-string (field ProbeSet alias))
                                          #\;)))
    (set gn:unigene (field ProbeSet UniGeneId))
    (set gn:generif (ontology 'generif: (field ProbeSet GeneId)))
    (set gn:genbank (field ProbeSet GenbankId))
    (set gn:blatSeq (sanitize-rdf-string
                     (string-trim-both (field ProbeSet BlatSeq))))
    (set gn:targetSeq (sanitize-rdf-string (field ProbeSet TargetSeq)))
    (set gn:omim
         (ontology
          'omim:
          (field ("IF(ProbeSet.OMIM REGEXP '^-?[0-9]+$' > 0, ProbeSet.OMIM, '')" OMIM))))
    (set gn:RefSeq_TranscriptId (field ProbeSet RefSeq_TranscriptId))
    (set gn:uniProtReference (ontology 'uniprot:
                                        (field ProbeSet UniProtID)))))



(call-with-target-database
 %connection-settings
 (lambda (db)
   (with-output-to-file (string-append %dump-directory "dump-probeset.ttl")
     (lambda ()
       (prefix "dct:" "<http://purl.org/dc/terms/>")
       (prefix "foaf:" "<http://xmlns.com/foaf/0.1/>")
       (prefix "generif:" "<http://www.ncbi.nlm.nih.gov/gene?cmd=Retrieve&dopt=Graphics&list_uids=>")
       (prefix "gn:" "<http://genenetwork.org/>")
       (prefix "owl:" "<http://www.w3.org/2002/07/owl#>")
       (prefix "phenotype:" "<http://genenetwork.org/phenotype/>")
       (prefix "pubmed:" "<http://rdf.ncbi.nlm.nih.gov/pubmed/>")
       (prefix "rdf:" "<http://www.w3.org/1999/02/22-rdf-syntax-ns#>")
       (prefix "rdfs:" "<http://www.w3.org/2000/01/rdf-schema#>")
       (prefix "taxon:" "<http://purl.uniprot.org/taxonomy/>")
       (prefix "uniprot:" "<http://purl.uniprot.org/uniprot/>")
       (prefix "up:" "<http://purl.uniprot.org/core/>")
       (prefix "xsd:" "<http://www.w3.org/2001/XMLSchema#>")
       (prefix "probeset:" "<http://genenetwork.org/probeset/>")
       (newline)
       (dump-probeset db))
     #:encoding "utf8")))
