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
           (left-join ProbeSetXRef "ON ProbeSetXRef.ProbeSetId = ProbeSet.Id")
           (left-join ProbeSetFreeze "ON ProbeSetXRef.ProbeSetFreezeId = ProbeSetFreeze.Id")
           (left-join GeneChip "ON GeneChip.Id = ProbeSet.ChipId")))
  (schema-triples
   (gn:name rdfs:range rdfs:Literal))
  (triples (ontology
            'probeset:
            (field ("IFNULL(ProbeSet.Name, ProbeSet.Id)"
                    name)))
    (set gn:probesetOfDataset
         (ontology
          'probeset:
          (regexp-substitute/global
           #f "[^A-Za-z0-9:]"
           (field ("IFNULL(ProbeSetFreeze.Name, '')" DatasetName))
           'pre "_" 'post)))
    (set gn:mean (annotate-field (field ("IFNULL(ProbeSetXRef.mean, '')" mean))
                                 '^^xsd:double))
    (set gn:se (annotate-field (field ("IFNULL(ProbeSetXRef.se, '')" se))
                               '^^xsd:double))
    (set gn:LRS (annotate-field (field ("IFNULL(ProbeSetXRef.LRS, '')" LRS))
                                '^^xsd:double))
    (set gn:pValue (annotate-field (field ("IFNULL(ProbeSetXRef.pValue, '')" pValue))
                                   '^^xsd:double))
    (set gn:additive (annotate-field (field ("IFNULL(ProbeSetXRef.additive, '')" additive))
                                     '^^xsd:double))
    (set gn:h2 (annotate-field (field ("IFNULL(ProbeSetXRef.h2, '')" h2))
                               '^^xsd:float))
    (set gn:locus (field ProbeSetXRef Locus))
    (set gn:chipOf (string->identifier "platform" (field GeneChip Name)))
    (set gn:name (field ProbeSet Name))
    (set gn:symbol (field ProbeSet Symbol))
    (set gn:description (field ProbeSet description))
    (set gn:chr (field ProbeSet Chr))
    (set gn:mb (annotate-field (field ("IFNULL(ProbeSet.Mb, '')" Mb)) '^^xsd:double))
    (set gn:alias (string-trim-both (field ProbeSet alias)))
    (set gn:generif (ontology 'generif: (field ProbeSet GeneId)))
    (set gn:genbank (ontology 'nuccore: (field ProbeSet GenbankId)))
    (set gn:snp (field ("IFNULL(ProbeSet.SNP, '')" SNP)))
    (set gn:blatSeq (string-trim-both (field ProbeSet BlatSeq)))
    (set gn:targetSeq (field ProbeSet TargetSeq))
    (set gn:unigene (field ProbeSet UniGeneId))
    (set gn:strandProbe (field ProbeSet Strand_Probe))
    (set gn:strandGene (field ProbeSet Strand_Gene))
    (set gn:omim (ontology 'omim: (field ProbeSet OMIM)))
    (set gn:comments (sanitize-rdf-string (field ProbeSet comments)))
    (set gn:targetRegion (field ProbeSet Probe_set_target_region))
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
    (set gn:flag (field ProbeSet flag))
    (set gn:chrNum (field ("IFNULL(ProbeSet.chr_num, '')" chr_num)))
    (set gn:nameNum (field ("IFNULL(ProbeSet.name_num, '')" name_num)))
    (set gn:probeTargetDescription (field ProbeSet Probe_Target_Description))
    (set gn:RefSeq_TranscriptId (ontology 'nuccore: (field ProbeSet RefSeq_TranscriptId)))
    (set gn:ENSEMBLGeneId (string-trim-both
                           (field ProbeSet ENSEMBLGeneId)))
    (set gn:Chr_mm8 (field ("IFNULL(ProbeSet.Chr_mm8, '')" Chr_mm8)))
    (set gn:Mb_mm8 (field ("IFNULL(ProbeSet.Mb_mm8, '')" Mb_mm8)))
    (set gn:probeSetBlatMbStart_mm8
         (annotate-field
          (field ("IFNULL(ProbeSet.Probe_set_Blat_Mb_start_mm8, '')" Probe_set_Blat_Mb_start_mm8))
          '^^xsd:double))
    (set gn:probeSetBlatMbEnd_mm8
         (annotate-field 
          (field ("IFNULL(ProbeSet.Probe_set_Blat_Mb_end_mm8, '')" Probe_set_Blat_Mb_end_mm8))
          '^^xsd:double))
    (set gn:homoloGeneID (ontology
                          'homologene:
                          (field ProbeSet HomoloGeneID)))
    (set gn:biotype_ENS (field ProbeSet Biotype_ENS))
    (set gn:proteinName (field ProbeSet ProteinName))
    (set gn:uniProtReference (ontology 'uniprot:
                                       (field ProbeSet UniProtID)))
    (set gn:flybase_Id (field ProbeSet Flybase_Id))
    (set gn:RGD_ID (field ("IFNULL(ProbeSet.RGD_ID, '')" RGD_ID)))
    (set gn:hgnc (ontology
                  'hgnc: (field ProbeSet HGNC_ID)))
    (set gn:HMDB_ID (field ProbeSet HMDB_ID))
    (set gn:confidence (field ("IFNULL(ProbeSet.Confidence, '')" Confidence)))
    (set gn:chebi_ID (ontology
                      'chebi:
                      (field
                       ("IFNULL(ProbeSet.ChEBI_ID, '')"
                        ChEBI_ID))))
    (set gn:CASNumber (field ProbeSet CAS_number))
    (set gn:PubChemID (ontology
                       'pubchem:
                       (field
                        ("IFNULL(ProbeSet.PubChem_ID, '')"
                         PubChem_ID))))
    (set gn:chemSpiderID (field ("IFNULL(ProbeSet.ChemSpider_ID, '')" ChemSpider_ID)))
    (set gn:uniiID (field ProbeSet UNII_ID))
    (set gn:ECNumber (field ProbeSet EC_number))
    (set gn:keggID (ontology 'kegg:
                             (field ProbeSet KEGG_ID)))
    (set gn:molecularWeight (annotate-field (field ProbeSet Molecular_Weight) '^^xsd:double))
    (set gn:nugowikiID (field ("IFNULL(ProbeSet.Nugowiki_ID, '')" Nugowiki_ID)))
    (set gn:type (field ProbeSet Type))
    (set gn:tissue (field ProbeSet Tissue))
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