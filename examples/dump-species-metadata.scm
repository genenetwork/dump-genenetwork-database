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



(define (remap-species-identifiers str)
  "This procedure remaps identifiers to standard binominal. Obviously this should
   be sorted by correcting the database!"
  (match str
    ["Fly (Drosophila melanogaster dm6)" "Drosophila melanogaster"]
    ["Oryzias latipes (Japanese medaka)" "Oryzias latipes"]
    ["Macaca mulatta" "Macaca nemestrina"]
    ["Bat (Glossophaga soricina)" "Glossophaga soricina"]
    [str str]))

(define-dump dump-species
  (tables (Species))
  (schema-triples
   (gnc:species rdf:type owl:Class)
   (gnt:name rdfs:range gnc:species)
   (gnt:binomialName rdfs:range gnc:species)
   (gnt:family rdfs:range gnc:species)
   (gnt:organism rdfs:range gnc:species))
  (triples
      (string->identifier "" (remap-species-identifiers (field Species Fullname))
                          #:separator ""
                          #:proc string-capitalize-first)
    (set rdf:type 'gnc:species)
    (set gnt:name (field Species SpeciesName))
    (set rdfs:label (field Species MenuName))
    (set gnt:binomialName (field Species FullName))
    (set gnt:family (field Species Family))
    (set gnt:organism (ontology 'taxon: (field Species TaxonomyId)))))

(define-dump dump-strain
  (tables (Strain
           (left-join Species "ON Strain.SpeciesId = Species.SpeciesId")))
  (schema-triples
   (gnc:strain rdf:subClassOf gnc:species)
   (gnt:species rdfs:domain gnc:strain)
   (gnt:alias rdfs:range gnc:strain)
   (gnt:symbol rdfs:range gnc:strain))
  (triples (string->identifier
            ""
            (regexp-substitute/global
             #f "[^A-Za-z0-9:]"
             (field ("CAST(CONVERT(BINARY CONVERT(Strain.Name USING latin1) USING utf8) AS VARCHAR(15000))" StrainName))
             'pre "_" 'post)
            #:separator ""
            #:proc string-capitalize-first)
    (set rdf:type 'gnc:strain)
    (set gnt:species
         (string->identifier "" (remap-species-identifiers (field Species Fullname))
                          #:separator ""
                          #:proc string-capitalize-first))
    ;; Name, and maybe a second name
    (set rdfs:label (sanitize-rdf-string (field Strain Name)))
    (set rdfs:label (sanitize-rdf-string (field ("IF ((Strain.Name2 != Strain.Name), Strain.Name2, '')" Name2))))
    (set gnt:alias (sanitize-rdf-string (field Strain Alias)))
    (set gnt:symbol (field Strain Symbol))))

(define-dump dump-mapping-method
  (tables (MappingMethod))
  (triples
      (string->identifier "mappingMethod" (field MappingMethod Name))
    (set rdf:type 'gnc:mappingMethod)))

(define-dump dump-inbred-set
  (tables (InbredSet
           (left-join Species "ON InbredSet.SpeciesId=Species.Id")
           (left-join MappingMethod
                       "ON InbredSet.MappingMethodId=MappingMethod.Id")))
  (schema-triples
   (gn-term:fullName rdfs:range rdfs:Literal)
   (gn-term:geneticType rdfs:range rdfs:Literal)
   (gn-term:inbredSetCode rdfs:range rdfs:Literal)
   (gn-term:inbredFamily rdfs:range rdfs:Literal)
   (gn-term:inbredSetOfSpecies rdfs:range gn:species)
   (gn-term:inbredSetType rdfs:range rdfs:Literal)
   (gn-term:phenotype rdfs:range gn-term:inbredSetType)
   (gn-term:genotype rdfs:range gn-term:inbredSetType)
   (gn-term:inbredSetOfMappingMethod rdfs:range gn-term:mappingMethod))
  (triples (string->identifier
            "" (field InbredSet Name)
            #:separator ""
            #:proc string-capitalize-first)
    (set rdf:type 'gnc:inbredSet)
    (set gn-term:binomialName (field InbredSet FullName))
    (set gn-term:geneticType (field InbredSet GeneticType))
    (set gn-term:inbredFamily (field InbredSet Family))
    (set gn-term:inbredSetOfMappingMethod (field MappingMethod Name))
    (set gn-term:inbredSetCode (field InbredSet InbredSetCode))
    (set gn-term:inbredSetOfSpecies
         (string->binomial-name
          (field Species FullName BinomialName)))
    (set gn-term:genotype
         (field ("IF ((SELECT PublishFreeze.Name FROM PublishFreeze WHERE PublishFreeze.InbredSetId = InbredSet.Id LIMIT 1) IS NOT NULL, 'Traits and Cofactors', '')" genotypeP)))
    (set gn-term:phenotype
         (field ("IF ((SELECT GenoFreeze.Name FROM GenoFreeze WHERE GenoFreeze.InbredSetId = InbredSet.Id LIMIT 1) IS NOT NULL, 'DNA Markers and SNPs', '')" phenotypeP)))))

(define-dump dump-avg-method
  ;; The Name and Normalization fields seem to be the same. Dump only
  ;; the Name field.
  (tables (AvgMethod))
  (schema-triples
   (gn-term:normalization rdfs:range rdfs:Literal))
  (triples (string->identifier "avgmethod" (field AvgMethod Name))
    (set rdf:type 'gnc:avgMethod)
    (set gn-term:normalization (field AvgMethod Normalization))))



(dump-with-documentation
 (name "Species Metadata")
 (connection %connection-settings)
 (table-metadata? #f)
 (prefixes
  '(("gn:" "<http://genenetwork.org/id/>")
    ("gnc:" "<http://genenetwork.org/category/>")
    ("gnt:" "<http://genenetwork.org/term/>")
    ("rdf:" "<http://www.w3.org/1999/02/22-rdf-syntax-ns#>")
    ("rdfs:" "<http://www.w3.org/2000/01/rdf-schema#>")
    ("taxon:" "<http://purl.uniprot.org/taxonomy/>")))
 (inputs
  (list dump-species
        dump-strain
        dump-mapping-method
        dump-avg-method))
 (outputs
  '(#:documentation "./docs/dump-species-metadata.md"
    #:rdf "./verified-data/dump-species-metadata.ttl")))
