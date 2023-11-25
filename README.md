# CDMiA (Categorical Data Migration Assessor)
CDMiA is a tool based on category theory to assess the impact of data migration on a schema, in multi-model systems.

## Categorical representation of models and schemas
Category theory provides useful constructs to represent data models and their constraints. To do so, specific morphisms and limits/colimits are used to represent identifiers, combinations of elements, or even combinations with constraints. 

Thus, a model is represented in a category, and a schema is a category along with a functor towards the category of its data model. 

## Assessing the impact of data migration on schemas
A migration is a functor from the schema towards the category of the destination data model. As functors preserve the structure of the source category by definition, it guarantees that the migration is consistent regarding data. Two kinds of migration can be evaluated with CDMiA: 
- template migration, for which the template is defined between the source model and the destination model as a functor. In which case the functor of the migration if built as a composition of the functor from the schema to its source data model and of the functor of the template;
- custom migration, that allows to define a different mapping for each element of the schema. Compared to the template migration, it allows to map elements of the same type in the schema differently in the destination model. 

To assess the impacts of a migration on a schema, CDMiA provides an output specifying the constraints and specificities that are preserved with respect to the source model, and the ones that must be created to respect the destination model. 

## Functionalities of CDMiA
CDMiA provides the following functionalities:
- displaying the categories of the data models along with their constraints and specificities;
- displaying schemas: their category, the category of their data model and the details of the mapping of the functor from the schema to its model;
- displaying defined migration templates;
- creating a migration template;
- applying a template or custom migration on a schema;
- displaying the impacts of a migration: the specificities and constraints of the source model that are preserved or lost, and those that must be created with respect to the destination model.

Its goal is to provide information regarding the impacts of a migration, in order to avoid costly migration operations is the schema is not compatible with the destination model.

## How to use CDMiA
CDMiA has been developed in Scala, and relies on JavaFX for the UI part. It can be run with Java 8 or higher.
```bash
java -jar CDMiA-0.1.0.jar
```

## Future developments
We plan to add several features to CDMiA:
- automatically extracting a schema from a data source into the application;
- executing a migration once it has been defined.