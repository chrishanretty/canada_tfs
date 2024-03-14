Canada TFS
===

This repository allows users to calculate the temporal focus of
speeches in the Canadian parliament between 1945 and 2018. 

It also models the future focus as a function of legislator age, and
cohort.

The repository is *not* self-contained. In order to run all of the code contained here, you will need to:

 - download the full (tarred, gzipped) Hansard dataset from [lipad.ca](https://www.lipad.ca/media/lipadcsv-1.1.0.tar.bz2) and extract it to the `data/` folder
 - download the (tarred, gzipped) data on members of the Commons from [lipad.ca](https://www.lipad.ca/media/ca-members.tar.gz) and extract it to `data/ca-members`
 
The repository also does not include the `mgcv` model object, since
this is larger than Github's file upload limits.
