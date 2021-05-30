# Script for assembling a Galactic Polymath interdisciplinary lesson plan
# ver 0.2.0

require(remotes)
install_github("galacticpolymath/galacticPubs")
library(galacticPubs)
googledrive::drive_auth() #need to authorize your account first time you use this

# Run this script to assemble the assets (e.g. graphs used for presentations, 
# videos, etc.) and data structures (JSON files) for publishing the lesson.

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# STEP 0: Define Global variables -------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# The lesson shortTitle, which will be added as a prefix to some file names
shortTitle<-"enterLessonShortTitle"
fullTitle <- "enterFullLessonTitle"
targetSubject<-c("ela","math","science","social studies") #Delete all but target



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# STEP 1: Run all subsidiary lesson scripts ---------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## PURPOSE: Run all scripts for making graphs and other visuals for 
##          the lesson. All .R files (except this one) should be in 
##          "scripts/"

# **Consider adding a comment explaining what each script does

runLessonScripts()



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# STEP 2: Compile alignment data --------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## PURPOSE: Compile the alignment matrix into a tidy tibble & output a 
##          JSON for Strapi
##
## DEPENDENT FILES: (which should be ready before running)
##  - meta/alignment-matrix.xlsx
##


# Aggregate standards alignment matrix notes and codes; merge with the alignments 
# master document from our standardX package
alignment<-compileStandards()



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# STEP 3: Output GP Learning Epaulette --------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## PURPOSE: Output subject percentage breakdown that will go at the top of the 
##          GP Sensible Lesson Plan

learningEpaulette(alignment,
                  targetSubj=targetSubject,
                  fileName=paste0(shortTitle,"_LearningEpaulette"))


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# STEP 4: Output GP Learning Chart ------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## PURPOSE: Output the custom windrose chart that will go in the 
##          standards section of the GP Sensible Lesson Plan

learningChart(alignment,
              targetSubj=targetSubject,
              caption=fullTitle,
              fileName=paste0(shortTitle,"_LearningChart"))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# STEP 5: Add/Update Google Drive Share Links for Lesson Artifacts-----------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## PURPOSE: Fill in/add for the first time share and download links to the teaching-
##          materials Excel spreadsheet
##
## DEPENDENT FILES: meta/teaching-materials.xlsx

#Delete Excel data to start over
# clearTeachingMatExcelData()

updateTeachingMatLinks(shortTitle)#c("quickPrep_feedback","remote","classroom")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# STEP 6: Compile Procedure -------------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## PURPOSE: Compile JSON of the procedure from XLSX spreadsheet
##
## DEPENDENT FILES: meta/procedure.xlsx

compileProcedure()




#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# STEP 7: Compile teaching material links and info --------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## PURPOSE: Convert spreadsheet teaching material data ( generated with 
##          updateTeachingMatLinks() ) into a JSON
##
## DEPENDENT FILES: meta/teaching-materials.xlsx

compileTeachingMat()






#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# STEP 8: Compile Acknowledgements JSON -------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Takes acknowledgment entries from "meta/acknowledgments.xlsx" & turns them into a
# structured JSON for Strapi
compileAcknowledgments()



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# STEP 9: Compile Version Info JSON -----------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Takes version info entries from "meta/version_info.xlsx" & turns them into a
# structured JSON for Strapi
compileVersions()


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# STEP 10: Combine JSONs ----------------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## PURPOSE: aggregate JSONS into 1 file for uploading to Strapi
##
## DEPENDENT FILES: 
# -meta/acknowledgments.json
# -meta/procedure.json
# -meta/standards_*.json
# -meta/teachingMaterials.json
# -meta/versions.json

compileJSON()


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# STEP 11: Any additional actions --------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



