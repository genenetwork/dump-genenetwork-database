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



(define-dump dump-species
  (tables (Species))
  (schema-triples
   (gn:name rdfs:range rdfs:Literal)
   (gn:displayName rdfs:range rdfs:Literal)
   (gn:binomialName rdfs:range rdfs:Literal)
   (gn:family rdfs:range rdfs:Literal))
  (triples (string->identifier "species" (field Species FullName))
    (set rdf:type 'gn:species)
    (set gn:name (field Species SpeciesName))
    (set gn:displayName (field Species MenuName))
    (set gn:binomialName (field Species FullName))
    (set gn:family (field Species Family))
    (set gn:organism (ontology 'taxon: (field Species TaxonomyId)))))

(define-dump dump-strain
  (tables (Strain
           (left-join Species "ON Strain.SpeciesId = Species.SpeciesId")))
  (schema-triples
   (gn:strainOfSpecies rdfs:domain gn:strain)
   (gn:strainOfSpecies rdfs:range gn:species)
   (gn:name rdfs:range rdfs:Literal)
   (gn:alias rdfs:range rdfs:Literal)
   (gn:symbol rdfs:range rdfs:Literal))
  (triples (string->identifier
            "strain"
            (regexp-substitute/global
             #f "[^A-Za-z0-9:]"
             (field ("CAST(CONVERT(BINARY CONVERT(Strain.Name USING latin1) USING utf8) AS VARCHAR(15000))" StrainName))
             'pre "_" 'post))
    (set rdf:type 'gn:strain)
    (set gn:strainOfSpecies
         (string->identifier "species" (field Species FullName)))
    ;; Name, and maybe a second name
    (set gn:name (sanitize-rdf-string (field Strain Name)))
    (set gn:name (sanitize-rdf-string (field Strain Name2)))
    (set gn:alias (sanitize-rdf-string (field Strain Alias)))
    (set gn:symbol (field Strain Symbol))))

(define-dump dump-mapping-method
  (tables (MappingMethod))
  (triples (string->identifier "mappingMethod" (field MappingMethod Name))
    (set rdf:type 'gn:mappingMethod)))

(define-dump dump-inbred-set
  (tables (InbredSet
           (left-join Species "ON InbredSet.SpeciesId=Species.Id")
           (left-join MappingMethod
                       "ON InbredSet.MappingMethodId=MappingMethod.Id")))
  (schema-triples
   (gn:fullName rdfs:range rdfs:Literal)
   (gn:geneticType rdfs:range rdfs:Literal)
   (gn:inbredSetCode rdfs:range rdfs:Literal)
   (gn:inbredFamily rdfs:range rdfs:Literal)
   (gn:inbredSetOfSpecies rdfs:range gn:species)
   (gn:inbredSetType rdfs:range rdfs:Literal)
   (gn:phenotype rdfs:range gn:inbredSetType)
   (gn:genotype rdfs:range gn:inbredSetType)
   (gn:inbredSetOfMappingMethod rdfs:range gn:mappingMethod))
  (triples (string->identifier "inbredSet" (field InbredSet Name))
    (set rdf:type 'gn:inbredSet)
    (set gn:binomialName (field InbredSet FullName))
    (set gn:geneticType (field InbredSet GeneticType))
    (set gn:inbredFamily (field InbredSet Family))
    (set gn:inbredSetOfMappingMethod (field MappingMethod Name))
    (set gn:inbredSetCode (field InbredSet InbredSetCode))
    (set gn:inbredSetOfSpecies
         (string->identifier "species" (field Species FullName BinomialName)))
    (set gn:genotype
         (field ("IF ((SELECT PublishFreeze.Name FROM PublishFreeze WHERE PublishFreeze.InbredSetId = InbredSet.Id LIMIT 1) IS NOT NULL, 'Traits and Cofactors', '')" genotypeP)))
    (set gn:phenotype
         (field ("IF ((SELECT GenoFreeze.Name FROM GenoFreeze WHERE GenoFreeze.InbredSetId = InbredSet.Id LIMIT 1) IS NOT NULL, 'DNA Markers and SNPs', '')" phenotypeP)))))

(define-dump dump-avg-method
  ;; The Name and Normalization fields seem to be the same. Dump only
  ;; the Name field.
  (tables (AvgMethod))
  (schema-triples
   (gn:name rdfs:range rdfs:Literal))
  (triples (string->identifier "avgmethod" (field AvgMethod Name))
    (set rdf:type 'gn:avgMethod)
    (set gn:name (field AvgMethod Name))))



(dump-with-documentation
 (name "Species Metadata")
 (connection %connection-settings)
 (table-metadata? #f)
 (prefixes
  (("rdf:" "<http://www.w3.org/1999/02/22-rdf-syntax-ns#>")
   ("rdfs:" "<http://www.w3.org/2000/01/rdf-schema#>")
   ("gn:" "<http://genenetwork.org/terms/>")
   ("taxon:" "<http://purl.uniprot.org/taxonomy/>")))
 (inputs
  (dump-species
   dump-strain
   dump-mapping-method
   dump-avg-method))
 (outputs
  (#:documentation "docs/dump-species-metadata.md"
   #:rdf "./verified-data/dump-species-metadata.ttl")))
