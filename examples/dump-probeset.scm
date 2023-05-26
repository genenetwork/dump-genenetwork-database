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
   (gn:name rdfs:range rdfs:Literal))
  (triples (ontology
            'probeset:
            (field ("IFNULL(ProbeSet.Name, ProbeSet.Id)"
                    name)))
    (set gn:chipOf (string->identifier "platform" (field GeneChip Name)))
    (set gn:name (field ProbeSet Name))
    (set gn:symbol (field ProbeSet Symbol))
    (set gn:description (field ProbeSet description))
    (set gn:chr (field ProbeSet Chr))
    (set gn:mb (annotate-field (field ("IFNULL(ProbeSet.Mb, '')" Mb)) '^^xsd:double))
    (multiset gn:alias (map string-trim-both
                            (string-split (sanitize-rdf-string (field ProbeSet alias))
                                          #\;)))
    (set gn:generif (ontology 'generif: (field ProbeSet GeneId)))
    (set gn:genbank (ontology 'nuccore: (field ProbeSet GenbankId)))
    (set gn:snp (field ("IFNULL(ProbeSet.SNP, '')" SNP)))
    (set gn:blatSeq (string-trim-both (field ProbeSet BlatSeq)))
    (set gn:targetSeq (field ProbeSet TargetSeq))
    (set gn:omim (ontology 'omim: (field ProbeSet OMIM)))
    (set gn:specificity (annotate-field
                         (field ("IFNULL(ProbeSet.Probe_set_specificity, '')" Probe_set_specificity))
                         '^^xsd:double))
    (set gn:blatScore (annotate-field
                       (field ("IFNULL(ProbeSet.Probe_set_BLAT_score, '')" Probe_set_BLAT_score))
                       '^^xsd:double))
    (set gn:blatMbStart (annotate-field
                         (field ("IFNULL(ProbeSet.Probe_set_Blat_Mb_start, '')" Probe_set_Blat_Mb_start))
                         '^^xsd:double))
    (set gn:blatMbend (annotate-field
                       (field ("IFNULL(ProbeSet.Probe_set_Blat_Mb_end, '')" Probe_set_Blat_Mb_end))
                       '^^xsd:double))
    (set gn:strand (field ProbeSet Probe_set_strand))
    (set gn:chrNum (field ("IFNULL(ProbeSet.chr_num, '')" chr_num)))
    (set gn:nameNum (field ("IFNULL(ProbeSet.name_num, '')" name_num)))
    (set gn:RefSeq_TranscriptId (ontology 'nuccore: (field ProbeSet RefSeq_TranscriptId)))
    (set gn:Chr_mm8 (field ("IFNULL(ProbeSet.Chr_mm8, '')" Chr_mm8)))
    (set gn:Mb_mm8 (field ("IFNULL(ProbeSet.Mb_mm8, '')" Mb_mm8)))
    (set gn:uniProtReference (ontology 'uniprot:
                                       (field ProbeSet UniProtID)))
    (set gn:PubChemID (ontology
                       'pubchem:
                       (field
                        ("IFNULL(ProbeSet.PubChem_ID, '')"
                         PubChem_ID))))
    (multiset gn:tissue (map string-trim-both
                        (string-split
                         (field ("IFNULL(ProbeSet.Tissue, '')" Tissue))
                         #\,)))
    (set gn:primaryName (field ProbeSet PrimaryName))
    (set gn:secondaryNames (field ProbeSet SecondaryNames))
    (set gn:peptideSequence (field ProbeSet PeptideSequence))))



(call-with-target-database
 %connection-settings
 (lambda (db)
   (with-output-to-file (string-append %dump-directory "dump-probeset.ttl")
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
       (prefix "taxon:" "<http://purl.uniprot.org/taxonomy/>")
       (prefix "uniprot:" "<http://purl.uniprot.org/uniprot/>")
       (prefix "up:" "<http://purl.uniprot.org/core/>")
       (prefix "xsd:" "<http://www.w3.org/2001/XMLSchema#>")
       (prefix "probeset:" "<http://genenetwork.org/probeset/>")
       (newline)
       (dump-probeset db))
     #:encoding "utf8")))
