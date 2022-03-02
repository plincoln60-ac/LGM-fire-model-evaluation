##November 8th 2021: Composite code to calculate npp 
##Note ORCHIDEE has been converted from a day to month fraction 
#Write all files to same directory
#SPITFIRE LGM

cp /Volumes/PL\ SSD/Fire\ models\ Jan\ 2021/SPITFIRE/LGM\ SF1/LPJ-GUESS-SPITFIRE_LGM_npp.nc /Users/paullincoln/Dropbox/2021/Research/RPD\ LGM\ for\ model\ comparison/November_21_new_references/SPITFIRE/LPJ-GUESS-SPITFIRE_LGM_npp.nc 
#SPITFIRE SF2
cp /Volumes/PL\ SSD/Fire\ models\ Jan\ 2021/SPITFIRE/SF2\ FLa/LPJ-GUESS-SPITFIRE_LGM_Reference_npp.nc /Users/paullincoln/Dropbox/2021/Research/RPD\ LGM\ for\ model\ comparison/November_21_new_references/SPITFIRE/LPJ-GUESS-SPITFIRE_LGM_Reference_npp.nc 

cd /Users/paullincoln/Dropbox/2021/Research/RPD\ LGM\ for\ model\ comparison/November_21_new_references/SPITFIRE/
#convert seconds to days & months

cdo -mulc,86400 LPJ-GUESS-SPITFIRE_LGM_npp.nc outfile.nc
cdo  -selmon,4,6,9,11 outfile.nc 30daymons1.nc 
cdo -mulc,30 30daymons1.nc 30daymons.nc
rm 30daymons1.nc
#31 day months
cdo -selmon,1,3,5,7,8,10,12 outfile.nc 31daymons1.nc 
cdo -mulc,31 31daymons1.nc 31daymons.nc
rm 31daymons1.nc
#28 day months
cdo -selmon,2 outfile.nc 28daymons1.nc 
cdo -mulc,28 28daymons1.nc 28daymons.nc
rm 28daymons1.nc outfile.nc 
cdo mergetime 30daymons.nc 31daymons.nc 28daymons.nc outfile2.nc
cdo timselsum,12 outfile2.nc outfile3.nc 
cdo timmean outfile3.nc SPITFIRE_LGM_npp.nc
rm 30daymons.nc 31daymons.nc 28daymons.nc outfile2.nc outfile3.nc


cdo -mulc,86400 -seltimestep,3001/3240 LPJ-GUESS-SPITFIRE_LGM_Reference_npp.nc outfile.nc

cdo  -selmon,4,6,9,11 outfile.nc 30daymons1.nc 
cdo -mulc,30 30daymons1.nc 30daymons.nc
rm 30daymons1.nc
#31 day months
cdo -selmon,1,3,5,7,8,10,12 outfile.nc 31daymons1.nc 
cdo -mulc,31 31daymons1.nc 31daymons.nc
rm 31daymons1.nc
#28 day months
cdo -selmon,2 outfile.nc 28daymons1.nc 
cdo -mulc,28 28daymons1.nc 28daymons.nc
rm 28daymons1.nc outfile.nc

cdo mergetime 30daymons.nc 31daymons.nc 28daymons.nc outfile2.nc
cdo timselsum,12 outfile2.nc outfile3.nc 
cdo timmean outfile3.nc SPITFIRE_LGM_Reference_npp.nc


rm 30daymons.nc 31daymons.nc 28daymons.nc outfile2.nc outfile3.nc


cdo griddes LPJ-GUESS-SPITFIRE_LGM_Reference_npp.nc  > SPITFIRE.grd
cdo remapbil,SPITFIRE.grd SPITFIRE_LGM_npp.nc SPITFIRE_LGM_npp1.nc

cdo -sub SPITFIRE_LGM_npp1.nc  SPITFIRE_LGM_Reference_npp.nc SPITFIRE_1951_1970_npp_diff.nc

rm SPITFIRE_LGM_Reference_npp.nc SPITFIRE_LGM_npp.nc SPITFIRE_LGM_npp1.nc LPJ-GUESS-SPITFIRE_LGM_npp1.nc

#SIMFIRE LGM
cp /Volumes/PL\ SSD/Fire\ models\ Jan\ 2021/SIMFIRE-BLAZE/LGM\ SF1/LPJ-GUESS-SIMFIRE_LGM_SF1_npp.nc /Users/paullincoln/Dropbox/2021/Research/RPD\ LGM\ for\ model\ comparison/November_21_new_references/SIMFIRE/LPJ-GUESS-SIMFIRE_LGM_SF1_npp.nc

#SIMFIRE SF2
##scale for landuse 
cd /Volumes/PL\ SSD/Fire\ models\ Jan\ 2021/SIMFIRE-BLAZE/v2\ FLa 
cdo -seltimestep,252/271  LPJ-GUESS-BLAZE_SF2_FLa_landCoverFrac2.nc lcf.nc
cdo -vertsum -sellevel,3/13 lcf.nc natsum.nc
cdo -vertsum lcf.nc totalsum.nc
cdo expr,'nppreground=1-landCoverFrac;' totalsum.nc nppreground.nc
cdo add natsum.nc nppreground.nc total_natural.nc
cdo expr,'scale=1/landCoverFrac;' total_natural.nc scalev2.nc
rm natsum.nc nppreground.nc totalsum.nc total_natural.nc lcf.nc
cp ./scalev2.nc /Users/paullincoln/Dropbox/2021/Research/RPD\ LGM\ for\ model\ comparison/November_21_new_references/SIMFIRE/scalev2.nc


cd /Users/paullincoln/Dropbox/2021/Research/RPD\ LGM\ for\ model\ comparison/November_21_new_references/SIMFIRE
cp /Volumes/PL\ SSD/Fire\ models\ Jan\ 2021/SIMFIRE-BLAZE/v2\ FLa/LPJ-GUESS-BLAZE_SF2_FLa_npp.nc  ./LPJ-GUESS-BLAZE_SF2_FLav2_npp.nc
cp /Volumes/PL\ SSD/Fire\ models\ Jan\ 2021/SIMFIRE-BLAZE/LGM\ SF1/LPJ-GUESS-SIMFIRE_LGM_SF1_npp.nc  ./LPJ-GUESS-SIMFIRE_LGM_SF1_npp.nc 


cdo -mulc,86400 -seltimestep,121/1200 LPJ-GUESS-SIMFIRE_LGM_SF1_npp.nc outfile.nc
cdo  -selmon,4,6,9,11 outfile.nc 30daymons1.nc 
cdo -mulc,30 30daymons1.nc 30daymons.nc
rm 30daymons1.nc
#31 day months
cdo -selmon,1,3,5,7,8,10,12 outfile.nc 31daymons1.nc 
cdo -mulc,31 31daymons1.nc 31daymons.nc
rm 31daymons1.nc
#28 day months
cdo -selmon,2 outfile.nc 28daymons1.nc 
cdo -mulc,28 28daymons1.nc 28daymons.nc
rm 28daymons1.nc outfile.nc 
cdo mergetime 30daymons.nc 31daymons.nc 28daymons.nc outfile2.nc
cdo timselsum,12 outfile2.nc outfile3.nc 
cdo timmean outfile3.nc SIMFIRE_LGM_npp.nc
rm 30daymons.nc 31daymons.nc 28daymons.nc outfile2.nc outfile3.nc


cdo -mulc,86400 -seltimestep,1/252 LPJ-GUESS-BLAZE_SF2_FLav2_npp.nc outfile.nc

cdo  -selmon,4,6,9,11 outfile.nc 30daymons1.nc 
cdo -mulc,30 30daymons1.nc 30daymons.nc
rm 30daymons1.nc
#31 day months
cdo -selmon,1,3,5,7,8,10,12 outfile.nc 31daymons1.nc 
cdo -mulc,31 31daymons1.nc 31daymons.nc
rm 31daymons1.nc
#28 day months
cdo -selmon,2 outfile.nc 28daymons1.nc 
cdo -mulc,28 28daymons1.nc 28daymons.nc
rm 28daymons1.nc outfile.nc

cdo mergetime 30daymons.nc 31daymons.nc 28daymons.nc outfile2.nc
cdo timselsum,12 outfile2.nc outfile3.nc 
cdo timmean outfile3.nc SIMFIRE_npp_1951_1970.nc
rm 30daymons.nc 31daymons.nc 28daymons.nc outfile2.nc outfile3.nc


cdo -sub SIMFIRE_LGM_npp.nc SIMFIRE_npp_1951_1970.nc SIMFIRE_1951_1970_npp_diff.nc
rm SIMFIRE_LGM_npp.nc SIMFIRE_npp_1951_1970.nc

#LPJ LM LGM

cd /Volumes/PL\ SSD/Fire\ models\ Jan\ 2021/LPJLMfire/
cdo select,name=NPP FireMIP_1700_2013.nc LPJ_Ref_npp.nc
cdo select,name=NPP FireMIP_natural.nc LPJ_LGM_npp.nc


mv /Volumes/PL\ SSD/Fire\ models\ Jan\ 2021/LPJLMfire/LPJ_LGM_npp.nc /Users/paullincoln/Dropbox/2021/Research/RPD\ LGM\ for\ model\ comparison/November_21_new_references/LPJ/LPJ_LGM_npp.nc

#LPJ LM SF2

mv /Volumes/PL\ SSD/Fire\ models\ Jan\ 2021/LPJLMfire/LPJ_Ref_npp.nc /Users/paullincoln/Dropbox/2021/Research/RPD\ LGM\ for\ model\ comparison/November_21_new_references/LPJ/LPJ_Ref_npp.nc

cd /Users/paullincoln/Dropbox/2021/Research/RPD\ LGM\ for\ model\ comparison/November_21_new_references/LPJ

cdo timmean -seltimestep,252/271  LPJ_Ref_npp.nc  LPJLM_npp_1951_1970.nc 
cdo timmean -seltimestep,51/141 LPJ_LGM_npp.nc LGM_npp.nc



cdo -sub LGM_npp.nc LPJLM_npp_1951_1970.nc outfile.nc
cdo -divc,1000 outfile.nc LPJLM_1951_1970_npp_diff.nc
rm outfile.nc LPJLM_npp_1951_1970.nc LGM_npp.nc


#ORCHIDEE SF2 note that npp values are daily for each timestep so have been converted to monthly values first
#get scale

cd /Volumes/PL\ SSD/Fire\ models\ Jan\ 2021/ORCHIDEE/SF2\ LCC

### need to manually extract this file if it doesn't work. I've deleted irrelevant timesteps so only 1901 to 1920 are left
tar -xzvf landCoverFrac.tar.gz 
cd ./landCoverFrac
rm landCoverFrac_2*.nc landCoverFrac_190* landCoverFrac_193* landCoverFrac_194*.nc landCoverFrac_192*.nc landCoverFrac_191*.nc landCoverFrac_198*.nc landCoverFrac_199*.nc landCoverFrac_17*.nc landCoverFrac_18*.nc landCoverFrac_1950.nc landCoverFrac_1971.nc landCoverFrac_1972.nc landCoverFrac_1973.nc landCoverFrac_1974.nc landCoverFrac_1975.nc landCoverFrac_1976.nc landCoverFrac_1977.nc landCoverFrac_1978.nc landCoverFrac_1979.nc


cdo mergetime *.nc outfile.nc
rm landCoverFrac_*.nc

cdo -vertsum -sellevel,1/11 outfile.nc natsum.nc
cdo -divc,100 natsum.nc natsumfrac.nc
cdo expr,'scale=1/landCoverFrac;' natsumfrac.nc scale.nc
cp ./scale.nc /Users/paullincoln/Dropbox/2021/Research/RPD\ LGM\ for\ model\ comparison/November_21_new_references/ORCHIDEE/scale.nc
cd ../
rm -r landCoverFrac


#extract monthly values
cd /Volumes/PL\ SSD/Fire\ models\ Jan\ 2021/ORCHIDEE/SF2\ LCC
rm -r npp
tar -xzvf npp.tar.gz
cd ./npp
rm npp_2*.nc npp_198*.nc npp_199*.nc npp_1950.nc npp_1971.nc npp_1972.nc npp_1973.nc npp_1974.nc npp_1975.nc npp_1976.nc npp_1977.nc npp_1978.nc npp_1979.nc

cdo -mulc,86400 -mergetime *.nc outfile.nc

#cdo -seltimestep,2412/2651 time.nc outfile.nc
rm npp_*.nc 


cdo  -selmon,4,6,9,11 outfile.nc 30daymons1.nc 
cdo -mulc,30 30daymons1.nc 30daymons.nc
rm 30daymons1.nc
#31 day months
cdo -selmon,1,3,5,7,8,10,12 outfile.nc 31daymons1.nc 
cdo -mulc,31 31daymons1.nc 31daymons.nc
rm 31daymons1.nc
#28 day months
cdo -selmon,2 outfile.nc 28daymons1.nc 
cdo -mulc,28 28daymons1.nc 28daymons.nc
rm 28daymons1.nc

cdo timmean -vertsum -mergetime 30daymons.nc 31daymons.nc 28daymons.nc ORCHIDEE_npp.nc
rm 30daymons.nc 31daymons.nc 28daymons.nc outfile.nc

mv ./ORCHIDEE_npp.nc /Users/paullincoln/Dropbox/2021/Research/RPD\ LGM\ for\ model\ comparison/November_21_new_references/ORCHIDEE/ORCHIDEE_SF2_npp.nc
cd ../
rm -r npp
#LGM values have already been georeffed using the R code in the LGM file

cd  /Volumes/PL\ SSD/Fire\ models\ Jan\ 2021/ORCHIDEE/Georeffed
####presuming here that Jan, not Feb is the first month
cdo -settaxis,1900-02-01,12:00:00,1mon ORCHIDEE_LGM_npp_Nov21.nc outfile1.nc
cdo seltimestep,121/1200 outfile1.nc outfile.nc
rm outfile1.nc outfile2.nc

#30 day months
cdo  -selmon,4,6,9,11 outfile.nc 30daymons1.nc 
cdo -mulc,30 30daymons1.nc 30daymons.nc
rm 30daymons1.nc
#31 day months
cdo -selmon,1,3,5,7,8,10,12 outfile.nc 31daymons1.nc 
cdo -mulc,31 31daymons1.nc 31daymons.nc
rm 31daymons1.nc
#28 day months
cdo -selmon,2 outfile.nc 28daymons1.nc 
cdo -mulc,28 28daymons1.nc 28daymons.nc
rm 28daymons1.nc
cdo -timselsum,12 -mergetime 30daymons.nc 31daymons.nc 28daymons.nc ORCHIDEE_LGM_npp1.nc
cdo -divc,1000 ORCHIDEE_LGM_npp1.nc ORCHIDEE_LGM_npp2.nc

cdo timmean ORCHIDEE_LGM_npp2.nc ORCHIDEE_LGM_npp.nc


rm 30daymons.nc 31daymons.nc 28daymons.nc ORCHIDEE_LGM_npp1.nc outfile.nc  ORCHIDEE_LGM_npp2.nc
mv ./ORCHIDEE_LGM_npp.nc /Users/paullincoln/Dropbox/2021/Research/RPD\ LGM\ for\ model\ comparison/November_21_new_references/ORCHIDEE/ORCHIDEE_LGM_npp.nc





cd /Users/paullincoln/Dropbox/2021/Research/RPD\ LGM\ for\ model\ comparison/November_21_new_references/ORCHIDEE/

#remap LGM model
cdo griddes ORCHIDEE_SF2_npp.nc > ORCHIDEE.grd
cdo remapbil,ORCHIDEE.grd ORCHIDEE_LGM_npp.nc ORCHIDEE_LGM_npp1.nc


#calculate difference
cdo -sub ORCHIDEE_LGM_npp1.nc ORCHIDEE_SF2_npp.nc ORCHIDEE_1951_1970_npp_diff.nc

rm ORCHIDEE_LGM_npp.nc ORCHIDEE_SF2_npp.nc




cd ../
mkdir ./Model\ means
cd ./Model\ means
cp /Users/paullincoln/Dropbox/2021/Research/RPD\ LGM\ for\ model\ comparison/November_21_new_references/SPITFIRE/SPITFIRE_1951_1970_npp_diff.nc ./SPITFIRE_1951_1970_npp_diff.nc 
cp /Users/paullincoln/Dropbox/2021/Research/RPD\ LGM\ for\ model\ comparison/November_21_new_references/LPJ/LPJLM_1951_1970_npp_diff.nc ./LPJLM_1951_1970_npp_diff.nc
cp /Users/paullincoln/Dropbox/2021/Research/RPD\ LGM\ for\ model\ comparison/November_21_new_references/ORCHIDEE/ORCHIDEE_1951_1970_npp_diff.nc ./ORCHIDEE_1951_1970_npp_diff.nc 
cp /Users/paullincoln/Dropbox/2021/Research/RPD\ LGM\ for\ model\ comparison/November_21_new_references/SIMFIRE/SIMFIRE_1951_1970_npp_diff.nc ./SIMFIRE_1951_1970_npp_diff.nc

cdo griddes SPITFIRE_1951_1970_npp_diff.nc > SPITFIRE.grd

cdo -remapbil,SPITFIRE.grd LPJLM_1951_1970_npp_diff.nc LPJLM_1951_1970_npp_diff1.nc
cdo remapbil,SPITFIRE.grd  ORCHIDEE_1951_1970_npp_diff.nc ORCHIDEE_1951_1970_npp_diff1.nc
cdo remapbil,SPITFIRE.grd  SIMFIRE_1951_1970_npp_diff.nc SIMFIRE_1951_1970_npp_diff1.nc

rm LPJLM_1951_1970_npp_diff2.nc LPJLM_1951_1970_npp_diff.nc ORCHIDEE_1951_1970_npp_diff.nc SIMFIRE_1951_1970_npp_diff.nc

cdo ensmean SPITFIRE_1951_1970_npp_diff.nc LPJLM_1951_1970_npp_diff1.nc ORCHIDEE_1951_1970_npp_diff1.nc SIMFIRE_1951_1970_npp_diff1.nc 1951_1970_mean_npp.nc
cdo ensstd SPITFIRE_1951_1970_npp_diff.nc LPJLM_1951_1970_npp_diff1.nc ORCHIDEE_1951_1970_npp_diff1.nc SIMFIRE_1951_1970_npp_diff1.nc 1951_1970_std_npp.nc
cdo ensvar SPITFIRE_1951_1970_npp_diff.nc LPJLM_1951_1970_npp_diff1.nc ORCHIDEE_1951_1970_npp_diff1.nc SIMFIRE_1951_1970_npp_diff1.nc 1951_1970_var_npp.nc

#move all files into 1950 folder
cd ../
cd ../
cd ./1951_1970_reference
mkdir ./npp
cd ./npp
mv  -v /Users/paullincoln/Dropbox/2021/Research/RPD\ LGM\ for\ model\ comparison/November_21_new_references/* ./












