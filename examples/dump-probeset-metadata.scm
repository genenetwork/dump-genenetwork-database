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


(define-dump dump-probeset-metadata
  (tables (ProbeSetXRef
           (left-join ProbeSet "ON ProbeSetXRef.ProbeSetId = ProbeSet.Id")
           (left-join ProbeSetFreeze "ON ProbeSetXRef.ProbeSetFreezeId = ProbeSetFreeze.Id"))
          "WHERE ProbeSetFreeze.public > 0 AND ProbeSetFreeze.confidentiality < 1")
  (schema-triples
   (gn:probesetData rdfs:range gn:probeset)
   (gnt:hasProbeset rdfs:range rdfs:Literal))
  (triples
      (string->identifier
       "probesetData"
       (field ("CONCAT(ProbeSetFreeze.Name,':',IFNULL(ProbeSet.Name, ProbeSet.Id))"
               ProbeSetName)))
    (set rdf:type 'gn:probesetData)
    (set gnt:hasProbeset
         (ontology
          'probeset:
          (regexp-substitute/global
           #f "[^A-Za-z0-9:]"
           (field ("IFNULL(ProbeSet.Name, ProbeSet.Id)"
                   name))
           'pre "_" 'post)))
    (set gnt:probesetOfDataset
         (ontology
          'probeset:
          (regexp-substitute/global #f "[^A-Za-z0-9:]"
                                    (field ProbeSetFreeze Name)
                                    'pre "_" 'post)))
    (set gnt:mean
         (annotate-field
          (field ("IFNULL(ProbeSetXRef.mean, '')" mean))
          '^^xsd:double))
    (set gnt:se
         (annotate-field
          (field ("IFNULL(ProbeSetXRef.se, '')" se))
          '^^xsd:double))
    (set gnt:locus (field ProbeSetXRef Locus))
    (set gn:LRS
         (annotate-field
          (field ("IFNULL(ProbeSetXRef.LRS, '')" LRS))
          '^^xsd:double))
    (set gnt:pValue
         (annotate-field
          (field ("IFNULL(ProbeSetXRef.pValue, '')" pValue))
          '^^xsd:double))
    (set gnt:additive
         (annotate-field
          (field ("IFNULL(ProbeSetXRef.additive, '')" additive))
          '^^xsd:double))
    (set gnt:h2
         (annotate-field
          (field ("IFNULL(ProbeSetXRef.h2, '')" h2))
          '^^xsd:float))))



(dump-with-documentation
 (name "Probeset Metadata")
 (connection %connection-settings)
 (table-metadata? #f)
 (prefixes
  '(("gn:" "<http://genenetwork.org/id/>")
    ("gnt:" "<http://genenetwork.org/id/>")
    ("rdf:" "<http://www.w3.org/1999/02/22-rdf-syntax-ns#>")
    ("rdfs:" "<http://www.w3.org/2000/01/rdf-schema#>")
    ("xsd:" "<http://www.w3.org/2001/XMLSchema#>")))
 (inputs
  (list dump-probeset-metadata))
 (outputs
  '(#:documentation "./docs/dump-probeset-metadata.md"
    #:rdf "./verified-data/dump-probeset-metadata.ttl")))
