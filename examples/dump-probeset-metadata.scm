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


(define-dump dump-probeset-metadata
  (tables (ProbeSetXRef
           (left-join ProbeSet "ON ProbeSetXRef.ProbeSetId = ProbeSet.Id")
           (left-join ProbeSetFreeze "ON ProbeSetXRef.ProbeSetFreezeId = ProbeSetFreeze.Id"))
          "WHERE ProbeSetFreeze.public > 0 AND ProbeSetFreeze.confidentiality < 1")
  (schema-triples
   (gn:probesetData rdfs:range gn:probeset)
   (gn:hasProbeset rdfs:range rdfs:Literal))
  (triples
      (string->identifier
       "probesetData"
       (field ("CONCAT(ProbeSetFreeze.Name,':',IFNULL(ProbeSet.Name, ProbeSet.Id))"
               ProbeSetName)))
    (set rdf:type 'gn:probesetData)
    (set gn:hasProbeset
         (ontology
          'probeset:
          (regexp-substitute/global
           #f "[^A-Za-z0-9:]"
           (field ("IFNULL(ProbeSet.Name, ProbeSet.Id)"
                   name))
           'pre "_" 'post)))
    (set gn:probesetOfDataset
         (ontology
          'probeset:
          (regexp-substitute/global #f "[^A-Za-z0-9:]"
                                    (field ProbeSetFreeze Name)
                                    'pre "_" 'post)))
    (set gn:mean
         (annotate-field
          (field ("IFNULL(ProbeSetXRef.mean, '')" mean))
          '^^xsd:double))
    (set gn:se
         (annotate-field
          (field ("IFNULL(ProbeSetXRef.se, '')" se))
          '^^xsd:double))
    (set gn:locus (field ProbeSetXRef Locus))
    (set gn:LRS
         (annotate-field
          (field ("IFNULL(ProbeSetXRef.LRS, '')" LRS))
          '^^xsd:double))
    (set gn:pValue
         (annotate-field
          (field ("IFNULL(ProbeSetXRef.pValue, '')" pValue))
          '^^xsd:double))
    (set gn:additive
         (annotate-field
          (field ("IFNULL(ProbeSetXRef.additive, '')" additive))
          '^^xsd:double))
    (set gn:h2
         (annotate-field
          (field ("IFNULL(ProbeSetXRef.h2, '')" h2))
          '^^xsd:float))))



(call-with-target-database
 %connection-settings
 (lambda (db)
   (with-output-to-file (string-append %dump-directory "dump-probeset-metadata.ttl")
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
       (dump-probeset-metadata db))
     #:encoding "utf8")))
