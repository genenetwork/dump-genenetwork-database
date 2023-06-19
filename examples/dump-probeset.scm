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


(define-dump dump-probeset-0
  (tables (ProbeSet
           (left-join GeneChip "ON GeneChip.Id = ProbeSet.ChipId"))
          "LIMIT 2000000 OFFSET 0")
  (schema-triples
   (gn:name rdfs:range rdfs:Literal)
   (gn:probeset rdfs:range rdfs:Literal))
  (triples (ontology
            'probeset:
            (string-trim-both
             (regexp-substitute/global
              #f "[^A-Za-z0-9:]"
              (field ("IFNULL(NULLIF(TRIM(ProbeSet.Name), ''), ProbeSet.Id)"
                      name))
              'pre "_" 'post)))
    (set rdf:type 'gn:probeset)
    (set gn:chipOf (string->identifier "platform" (field GeneChip Name)))
    (set gn:name (field ProbeSet Name))
    (set gn:symbol (delete-substrings (field ProbeSet Symbol) "\""))
    (set gn:description (sanitize-rdf-string
                         (field ProbeSet description)))
    (set gn:chr (field ProbeSet Chr))
    (set gn:mb (annotate-field (field ("IFNULL(ProbeSet.Mb, '')" Mb)) '^^xsd:double))
    ;; For now have the tissue, and alias as one line without
    ;; splitting to make the dump faster
    ;; (set gn:tissue (field ("IFNULL(ProbeSet.Tissue, '')" Tissue)))
    ;; (set gn:alias (field ProbeSet alias))
    ;; (set gn:generif (ontology 'generif: (field ProbeSet GeneId)))
    (set gn:blatSeq (sanitize-rdf-string
                     (string-trim-both (field ProbeSet BlatSeq))))
    (set gn:targetSeq (sanitize-rdf-string (field ProbeSet TargetSeq)))
    ;; (set gn:unigene (field ProbeSet UniGeneId))
    ;; (set gn:genbank (field ProbeSet GenbankId))
    ;; (set gn:omim (sanitize-rdf-string (string-trim-both (field ProbeSet OMIM))))
    ;; (set gn:RefSeq_TranscriptId (field ProbeSet RefSeq_TranscriptId))
    (set gn:uniProtReference (ontology 'uniprot:
                                       (field ProbeSet UniProtID)))))

(define-dump dump-probeset-1
  (tables (ProbeSet
           (left-join GeneChip "ON GeneChip.Id = ProbeSet.ChipId"))
          "LIMIT 2000000 OFFSET 2000000")
  (schema-triples
   (gn:name rdfs:range rdfs:Literal)
   (gn:probeset rdfs:range rdfs:Literal))
  (triples (ontology
            'probeset:
             (string-trim-both
             (regexp-substitute/global
              #f "[^A-Za-z0-9:]"
              (field ("IFNULL(NULLIF(TRIM(ProbeSet.Name), ''), ProbeSet.Id)"
                      name))
              'pre "_" 'post)))
    (set rdf:type 'gn:probeset)
    (set gn:chipOf (string->identifier "platform" (field GeneChip Name)))
    (set gn:name (field ProbeSet Name))
    (set gn:symbol (delete-substrings (field ProbeSet Symbol) "\""))
    (set gn:description (sanitize-rdf-string
                         (field ProbeSet description)))
    (set gn:chr (field ProbeSet Chr))
    (set gn:mb (annotate-field (field ("IFNULL(ProbeSet.Mb, '')" Mb)) '^^xsd:double))
    (set gn:blatSeq (sanitize-rdf-string
                     (string-trim-both (field ProbeSet BlatSeq))))
    (set gn:targetSeq (sanitize-rdf-string (field ProbeSet TargetSeq)))
    (set gn:uniProtReference (ontology 'uniprot:
                                       (field ProbeSet UniProtID)))))
(define-dump dump-probeset-2
  (tables (ProbeSet
           (left-join GeneChip "ON GeneChip.Id = ProbeSet.ChipId"))
          "WHERE ProbeSet.Name IS NOT NULL LIMIT 2000000 OFFSET 4000000")
  (schema-triples
   (gn:name rdfs:range rdfs:Literal)
   (gn:probeset rdfs:range rdfs:Literal))
  (triples (ontology
            'probeset:
            (string-trim-both
             (regexp-substitute/global
              #f "[^A-Za-z0-9:]"
              (field ("IFNULL(ProbeSet.Name, ProbeSet.Id)"
                      name))
              'pre "_" 'post)))
    (set rdf:type 'gn:probeset)
    (set gn:chipOf (string->identifier "platform" (field GeneChip Name)))
    (set gn:name (field ProbeSet Name))
    (set gn:symbol (delete-substrings (field ProbeSet Symbol) "\""))
    (set gn:description (sanitize-rdf-string
                         (field ProbeSet description)))
    (set gn:chr (field ProbeSet Chr))
    (set gn:mb (annotate-field (field ("IFNULL(ProbeSet.Mb, '')" Mb)) '^^xsd:double))
    (set gn:blatSeq (sanitize-rdf-string
                     (string-trim-both (field ProbeSet BlatSeq))))
    (set gn:targetSeq (sanitize-rdf-string (field ProbeSet TargetSeq)))
    (set gn:uniProtReference (ontology 'uniprot:
                                       (field ProbeSet UniProtID)))))



(call-with-target-database
 %connection-settings
 (lambda (db)
   (with-output-to-file (string-append %dump-directory "dump-probeset-0.ttl")
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
       (prefix "uniprot:" "<http://purl.uniprot.org/uniprot/>")
       (prefix "up:" "<http://purl.uniprot.org/core/>")
       (prefix "xsd:" "<http://www.w3.org/2001/XMLSchema#>")
       (prefix "probeset:" "<http://genenetwork.org/probeset/>")
       (newline)
       (dump-probeset-0 db))
     #:encoding "utf8")
   (with-output-to-file (string-append %dump-directory "dump-probeset-1.ttl")
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
       (prefix "uniprot:" "<http://purl.uniprot.org/uniprot/>")
       (prefix "up:" "<http://purl.uniprot.org/core/>")
       (prefix "xsd:" "<http://www.w3.org/2001/XMLSchema#>")
       (prefix "probeset:" "<http://genenetwork.org/probeset/>")
       (newline)
       (dump-probeset-1 db))
     #:encoding "utf8")
   (with-output-to-file (string-append %dump-directory "dump-probeset-2.ttl")
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
       (prefix "uniprot:" "<http://purl.uniprot.org/uniprot/>")
       (prefix "up:" "<http://purl.uniprot.org/core/>")
       (prefix "xsd:" "<http://www.w3.org/2001/XMLSchema#>")
       (prefix "probeset:" "<http://genenetwork.org/probeset/>")
       (newline)
       (dump-probeset-2 db))
     #:encoding "utf8")))
