cd /Users/paullincoln/Dropbox/2021/Research/RPD\ LGM\ for\ model\ comparison/November_21_new_references/
mkdir ./SPITFIRE
mkdir ./ORCHIDEE


#SPITFIRE LGM
cd /Volumes/PL\ SSD/Fire\ models\ Jan\ 2021/SPITFIRE/LGM\ SF1
cdo ensmean LPJ-GUESS-SPITFIRE_LGM_mFuel1hr.nc LPJ-GUESS-SPITFIRE_LGM_mFuel10hr.nc LPJ-GUESS-SPITFIRE_LGM_mFuel100hr.nc mFuel.nc
cdo timmean -divc,12 -seltimestep,1/90 -timselsum,12 mFuel.nc mFuel_annual.nc


cp ./mFuel_annual.nc /Users/paullincoln/Dropbox/2021/Research/RPD\ LGM\ for\ model\ comparison/November_21_new_references/SPITFIRE/SPITFIRE_LGM_mFuel.nc 

rm mFuel.nc mFuel_annual.nc
#SPITFIRE SF2
cd /Volumes/PL\ SSD/Fire\ models\ Jan\ 2021/SPITFIRE/SF2\ FLa/
cdo ensmean LPJ-GUESS-SPITFIRE_LGM_Reference_mFuel1hr.nc LPJ-GUESS-SPITFIRE_LGM_Reference_mFuel10hr.nc LPJ-GUESS-SPITFIRE_LGM_Reference_mFuel100hr.nc mFuel.nc
cdo timmean -divc,12 -seltimestep,251/270 -timselsum,12 mFuel.nc mFuel_annual.nc

cp ./mFuel_annual.nc /Users/paullincoln/Dropbox/2021/Research/RPD\ LGM\ for\ model\ comparison/November_21_new_references/SPITFIRE/SPITFIRE_mFuel.nc 
rm mFuel.nc mFuel_annual.nc
cd /Users/paullincoln/Dropbox/2021/Research/RPD\ LGM\ for\ model\ comparison/November_21_new_references/SPITFIRE/

cdo griddes SPITFIRE_mFuel.nc  > SPITFIRE.grd
cdo remapbil,SPITFIRE.grd SPITFIRE_LGM_mFuel.nc SPITFIRE_LGM_mFuel1.nc

cdo -sub SPITFIRE_LGM_mFuel1.nc SPITFIRE_mFuel.nc SPITFIRE_1951_1970_mFuel_diff.nc

rm SPITFIRE_LGM_mFuel.nc

#ORCHIDEE SF2 note that BA values are daily for each timestep so have been converted to monthly values first

#extract monthly values
cd /Volumes/PL\ SSD/Fire\ models\ Jan\ 2021/ORCHIDEE/SF2\ LCC
rm -r mFuel
tar -xzvf mFuel.tar.gz
cd ./mFuel
rm mFuel_20*.nc mFuel_198*.nc mFuel_199*.nc mFuel_1950.nc mFuel_1971.nc mFuel_1972.nc mFuel_1973.nc mFuel_1974.nc mFuel_1975.nc mFuel_1976.nc mFuel_1977.nc mFuel_1978.nc mFuel_1979.nc

cdo mergetime *.nc outfile1.nc
cdo -timselsum,12 outfile1.nc outfile.nc 
cdo timmean -divc,12 outfile.nc ORCHIDEE_SF2_mFuel.nc


rm mFuel_*.nc 


mv ./ORCHIDEE_SF2_mFuel.nc /Users/paullincoln/Dropbox/2021/Research/RPD\ LGM\ for\ model\ comparison/November_21_new_references/ORCHIDEE/ORCHIDEE_SF2_mFuel.nc
cd ../
rm -r mFuel


##LGM
cd  /Volumes/PL\ SSD/Fire\ models\ Jan\ 2021/ORCHIDEE/Georeffed
####presuming here that Jan, not Feb is the first month
cdo -settaxis,1900-02-01,12:00:00,1mon ORCHIDEE_LGM_mFuel_Nov21.nc outfile1.nc
cdo -timselsum,12 -seltimestep,121/1200 outfile1.nc outfile.nc 
cdo timmean -divc,12 outfile.nc ORCHIDEE_LGM_mFuel.nc

mv ./ORCHIDEE_LGM_mFuel.nc /Users/paullincoln/Dropbox/2021/Research/RPD\ LGM\ for\ model\ comparison/November_21_new_references/ORCHIDEE/ORCHIDEE_LGM_mFuel.nc
rm outfile.nc outfile1.nc
cd /Users/paullincoln/Dropbox/2021/Research/RPD\ LGM\ for\ model\ comparison/November_21_new_references/ORCHIDEE/

cdo griddes ORCHIDEE_SF2_mFuel.nc > ORCHIDEE.grd
cdo -remapbil,ORCHIDEE.grd ORCHIDEE_LGM_mFuel.nc ORCHIDEE_LGM_mFuel1.nc
rm ORCHIDEE_LGM_mFuel.nc

cdo  -sub ORCHIDEE_LGM_mFuel1.nc ORCHIDEE_SF2_mFuel.nc ORCHIDEE_1951_1970_mFuel_diff.nc
#calculate ensemble mean
cd ../
mkdir ./Model\ means
cd ./Model\ means
cp /Users/paullincoln/Dropbox/2021/Research/RPD\ LGM\ for\ model\ comparison/November_21_new_references/SPITFIRE/SPITFIRE_1951_1970_mFuel_diff.nc ./SPITFIRE_1951_1970_mFuel_diff.nc
cp /Users/paullincoln/Dropbox/2021/Research/RPD\ LGM\ for\ model\ comparison/November_21_new_references/ORCHIDEE/ORCHIDEE_1951_1970_mFuel_diff.nc ./ORCHIDEE_1951_1970_mFuel_diff.nc


cdo griddes SPITFIRE_1951_1970_mFuel_diff.nc > SPITFIRE.grd
cdo -remapbil,SPITFIRE.grd ORCHIDEE_1951_1970_mFuel_diff.nc ORCHIDEE_1951_1970_mFuel_diff1.nc

rm ORCHIDEE_1951_1970_mFuel_diff.nc 

cdo ensmean ORCHIDEE_1951_1970_mFuel_diff1.nc SPITFIRE_1951_1970_mFuel_diff.nc  1951_1970_mean_mFuel.nc
cdo ensstd ORCHIDEE_1951_1970_mFuel_diff1.nc SPITFIRE_1951_1970_mFuel_diff.nc 1951_1970_std_mFuel.nc
cdo ensvar ORCHIDEE_1951_1970_mFuel_diff1.nc SPITFIRE_1951_1970_mFuel_diff.nc 1951_1970_ensvar_mFuel.nc
rm ORCHIDEE_1951_1970_mFuel_diff1.nc SPITFIRE_1951_1970_mFuel_diff.nc

#move folders to 1950 folder


#move all files into 1950 folderπ∏
cd ../
cd ../
cd ./1951_1970_reference
mkdir ./mFuel
cd ./mFuel
mv  -v /Users/paullincoln/Dropbox/2021/Research/RPD\ LGM\ for\ model\ comparison/November_21_new_references/* ./




