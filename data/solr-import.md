# How to import people (json) into solr

## Download data
```
wget -O exercise.zip http://www.klarnaisrael.com/exercise
```
## start solr
```
$ cd SOLR_HOME
$ bin/solr start
$ bin/solr create -c people
```
## update solr schema
```
cp data/managed-schema SOLR_HOME/server/solr/people/conf/managed-schema
$ bin/solr restart
```
## import data
```
$ bin/post -c people PATH/data/people.json
```


## cleanup
$ bin/solr stop -all
$ rm -r server/solr/people
$ bin/solr start
$ bin/solr create -c people
