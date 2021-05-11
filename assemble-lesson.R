require(remotes)
install_github("galacticpolymath/galacticPubs")
library(galacticPubs)

# Run this script to assemble the assets (e.g. graphs used for presentations, 
# videos, etc.) and data structures (JSON files) for publishing the lesson.

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# STEP 0: Define Global variables -------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# The lesson shortTitle, which will be added as a prefix to some file names
shortTitle<-"enterLessonShortTitle"
fullTitle <- "enterFullLessonTitle"
targetSubject<-c("ela","math","science","social studies") #options= "math","ela","science","social studies"





#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# STEP 1: Run all subsidiary lesson scripts ---------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# *All scripts for making graphs and other visuals for the lesson should be in 
# "scripts/"


# **Add a comment explaining what each script does and source it
# source("scripts/lessonAssetModule.R")




#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# STEP 2: Compile alignment data --------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Compile the alignment matrix into a tidy tibble & output a JSON for Strapi

## Import the completed lesson alignment matrix from the meta subfolder
# Suggestion for the file to import
(likelyFilename<-list.files("meta/",pattern = "^[^~].*atrix.*")) 
f<-likelyFilename #use the suggested file, type in the path, or use f<-file.choose()

# Aggregate alignment matrix notes and codes; merge with the alignments master
# document from our standardX package
alignment<-compileAlignment()




#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# STEP 3: Output GP Learning Epaulette --------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Output subject percentage breakdown that will go at the top of the 
# GP Sensible Lesson Plan

learningEpaulette(alignment,
                  targetSubj=targetSubject,
                  fileName=paste0(shortTitle,"_LearningEpaulette"))





#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# STEP 4: Output GP Learning Chart ------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Output the custom windrose chart that will go in the standards section of 
# the GP Sensible Lesson Plan
learningChart(alignment,
              targetSubj=targetSubject,
              caption=fullTitle,
              fileName=paste0(shortTitle,"_LearningChart"))



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# STEP 5: Output lesson chunking visuals ------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# STEP 3: Compile Procedure -------------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Compile Procedure from XLSX spreadsheet

compileProcedure()



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# STEP 6: Compile Acknowledgements JSON -------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Takes acknowledgment entries from "meta/acknowledgments.xlsx" & turns them into a
# structured JSON for Strapi
compileAcknowledgments(fileName=paste0(shortTitle,"_acknowledgements.json"))





#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# STEP 7: Compile Version Info JSON -----------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Takes version info entries from "meta/version_info.xlsx" & turns them into a
# structured JSON for Strapi
compileVersions(fileName=paste0(shortTitle,"_version_info.json"))


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# STEP 8: Add Google Drive Share Links for Lesson Artifacts------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
googledrive::drive_auth() #need to authorize your account first time you use this
updateTeachingMatLinks(shortTitle)#c("quickPrep_feedback","remote","classroom")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# STEP 9: Any additional actions --------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
compileTeachingMat()





