

#pragma rtGlobals=1
#pragma hide=1
#pragma IgorVersion=6.21

//#if (1)
//	SetIgorOption poundDefine=PROC_INSTALLED?
//#endif
//#if(!v_flag)
menu "Macros",dynamic
		"Load Original Data/F5",LoadIgorText("")
		help={"Load Igor text file."}
		submenu "&Import and Export"
		help={"Import saved data from or export analyzed data to Igor text file."}
		"&Import Data/CF5",Import(SELECT,"")
		help={"Import analyzed data."}
		"&Export Data",Export(SELECT,0,"","",TRUE)
		help={"Export analyzed data."}
		"Export to &MSExcel",ExportExcel("","",TRUE)
		help={"Export analyzed data to Microsoft Excel file."}
		end
		"-"
		MenuItem("&Average/F9"),ShowAverage()
		help={"Show averaged waveform of specified wave(s)."}
		MenuItem("&Properties/F10"),StatsResult()
		help={"Analyze wave properties."}
		MenuItem("Co&rrelation"),Correlation("","")
		help={"Show correlation between specific waves."}
		"-"
		submenu "&mEPSC Analysis"
			help={"Analysis methods for mEPSC data."}
			MenuItem("&Frequency"),Frequency(0,inf)
			help={"Show mEPSC frequency."}
			"-"
			MenuItem("&Resume Judge/F8"),GetmEPSC(1)
			help={"Resume judging mEPSC."}
			"-"
			MenuItem("&Kill mEPSC"),KillmEPSC("",-1)
			help={"Kill certain mEPSC waves."}
			MenuItem("Kill &Orphan Waves"),KillSingle()
			help={"Kill waves with no paired signals."}
			"-"
			submenu "A&lignment Waves"
				help={"Align waves according to peak or differential time points."}
				MenuItem("Align to Peak"),PeakAlignment("*")
				help={"Temporal alignment according to peak time."}
				MenuItem("Align to Diff"),DiffAlignment("*")
				help={"Temporal alignment according to aifferneital peak time."}
			end
		end
		submenu "&EPSC Analysis"
			help={"Analysis methods for eEPSC data."}
			MenuItem("&Pair Pulse Ratio"),PairPulse()
			help={"Calculate paired-pulse ratio."}
			MenuItem("&RRP Size"),RRPSize(SELECT,SELECT)
			help={"Calculate readily releasable pool size."}
			MenuItem("&Steady Phase Analyses"),SteadyPhase(SELECT)
			help={"Do statistical analysis of properties of steady-state eEPSCs."}
			"-"
			MenuItem("Set &Train Config"),SetTrainConfig(SELECT)
			help={"Set parameters of stimulation trains."}
			"-"
			MenuItem("&Export Property Stats"),ExportStats(SELECT,"",FALSE)
			help={"Export statistics results."}
		end
		submenu "I&ca Analysis"
			help={"Analysis methods for calcium current data."}
			MenuItem("Get &Calcium Current"),GetCaCurrent()
			help={"Set parameters of stimulation protocol to extract calcium current."}
		end
		"-"
		submenu "&General Function"
			help={"Igor extension for batch works."}
			"-"
			MenuItem("&Concatenate Waves"),ConcatWave("",0,TRUE,TRUE,"")
			help={"Concatenate waves by names or patterns."}
			MenuItem("&Smooth Waves"),SmoothWave("",0,20,TRUE)
			help={"Smooth waves by names or patterns."}
			MenuItem("&Rename Waves"),RenameWave("","",0,0,TRUE)
			help={"Do batch raname of waves by names or patterns."}
			MenuItem("&Kill Waves"),BatchKill("",0)
			help={"Batch kill waves by names or patterns."}
		end
		"-"
		"C&onfiguration",SetConfig(TRUE)
		help={"Set procedure configuration."}
		MenuItem("Show &File Information"),ShowExpInfo()
		help={"Show configuration and information of the running experiment."}
		MenuItem("&Update Old File"),UpdateExist(0)
		help={"Update experiment file created by procedure of old versions."}
		"-"
//		submenu "A&dvanced Functions"
//		help={"Advanced functions."}
//			"&Customize",Customize()
//			help={"Customize the procedure file for your personal usage."}
//		end
//		"-"
		"&Help Context",Help("")
		help={"Show help of certain function."}
		"-;Disable mEPSC_v4.2",DisableProc()
		help={"Disable \"mEPSC_v4.2\" and stop it from automatically starting."}
end
//#endif
////////////////////Constant Declaration////////////////////////////

Override Constant TRUE = 1							//condition true
Override Constant FALSE = 0							//condition false
Override Constant ALL = -1							//index for all
Override Constant SELECT = -2						//index for select

////////////////////ErrorCode Definition////////////////////////////

Override Constant ERROR_SUCCESS = 0				//success
Override Constant ERROR_NOT_INITIALIZED = 1		//not initialized
Override Constant ERROR_NOT_NEEDED = 2				//not needed
Override Constant ERROR_NOT_ALLOWED = 3			//not allowed
Override Constant ERROR_IGNORED = 4				//ingored
Override Constant ERROR_DISABLED = 5				//disabled
Override Constant ERROR_INDEX_OUT_OF_RANGE = 6		//index out of range
Override Constant ERROR_QUICK_MODE = 7				//quick mode

Override Constant ERROR_USER_CANCELED = 101		//user cancelled
Override Constant ERROR_USER_ABORTED = 102			//user aborted
Override Constant ERROR_ZERO_OBJECT_LENGTH = 103	//invalid object length

Override Constant ERROR_INVALID_WAVE = 201			//invilid wave or wavename
Override Constant ERROR_INVALID_RES_WAVE = 202		//invilid reserved wave
Override Constant ERROR_WAVE_NOT_KILLED = 203		//wave still in use, not killed
Override Constant ERROR_NO_TARGET_WAVE = 204		//target wave unavailable

Override Constant ERROR_INVALID_WINDOW = 301		//invalid window or winname
Override Constant ERROR_WINDOW_NOT_KILLED = 302	//invalid window or winname

Override Constant ERROR_INVALID_FILE = 401			//invalid file or filename

Override Constant ERROR_INVALID_CONTROL = 501		//invalid control or controlname
Override Constant ERROR_INVALID_CTRL_TEXT = 502	//invalid control text

Override Constant ERROR_INVALID_FOLDER = 601		//invalid data folder

Override Constant ERROR_INVALID_PROCEDURE = 801	//invalid procedure
Override Constant ERROR_TOO_OLD_VERSION = 802		//too old proc version

Override Constant ERROR_MINI_TOO_NARROW = 1001
Override Constant ERROR_MINI_TOO_BIG = 1002
Override Constant ERROR_MINI_NOT_BACK = 1003
Override Constant ERROR_MINI_DXPFIT_ERR = 1004
Override Constant ERROR_MINI_LINFIT_ERR = 1005
Override Constant ERROR_MINI_LIKE_LINE = 1006
Override Constant ERROR_MINI_AUTO_JUDGE = 1007
Override Constant ERROR_MINI_SUCCESS = 1008


////////////////////Load File & Set Configuration///////////////////

Override Function Initial()
											//initializes the procedure, creates reserved waves global numeric and
											//_string variables.

											//details: Initial prepares environment for the sequential data loading and
											//_processing. It will be called by relevant functions before LoadIgorText and Import
											//_operations. If you want to manually execute loading operations or design
											//_your own functions based on this procedure, please make sure to explicitly
											//_use this function at first by either command line or program.
											//The global numeric flag isinit will be set to 1 if the procedure has been
											//_successfully initialized.

											//call: InitReserve, SetConfig, RecHistory
											//seealso: LoadIgorText, Import
	SetDataFolder root:					//Set current data folder
	variable/g isinit						//is a flag used to show whether procedure intialization process
											//_has been successfully finished or not. The functions and operations which depend
											//_on global settings can be usable only when isinit is set to a nonzero value (default 1).

	if(isinit==1)
		return ERROR_NOT_NEEDED
	endif
	RecHistory()



	InitReserve(ALL,FALSE)
	SetConfig(FALSE)						//Not show dialog

	isinit = 1								//initialization finished
	return ERROR_SUCCESS
End
Override Function InitReserve(idx,fmode)
											//creates or loads the reserved waves.
											//call: GetRecentPath
	variable idx							//waveIndex
											//is used to specify which wave to create or load. It is reserved for future
											//_us and the default value is ALL (-1).
	variable fmode							//InitMode
											//decides whether default channel map settings will be loaded or not.
											//_If the function is called during initialization process, the
											//_reserved channel map wave will be checked for loading. The values
											//_could be:
											//TRUE    Called by CheckUpdate.
											//FALSE    Called by Initial, check default channel map.
	variable mdflag
	if(fmode)
		ExperimentModified
		mdflag = v_flag
//		print "mdflag",v_flag
	endif
	String prewpref = "pre*"				//original pre wave name prefix
	String postwpref = "post*"			//original post wave name prefix

	String minipref = "mini*"				//mepsc wave name prefix
	String epscpref = "epsc*"				//eepsc wave name prefix
	String capref = "ca*"					//presynaptic wave for calcium influx measurement

	variable i,n

	string line = ""

	String plotmini = "Plot_mEPSC"
	string plotsmini = "Plot_Scaled_mEPSC"
	string plotfsmini = "Plot_Fit_Scaled_mEPSC"
	string plottimeamp = "Plot_Time_Amp"
	string plotstats = "Plot_Statistics"

	make/t/o/n=5 rnw						//is the text wave where the default window names
											//_for mEPSC judgement and property statistical
											//_result tables are saved.
											//It is a 5-element vector.
	wave/t rnw
	rnw = {plotmini,plotsmini,plotfsmini,plottimeamp,plotstats}
	n = 19

	make/t/n=(n,5)/o rn					//is the main text wave that contains most of the
											//_constant strings used for naming, grouping and
											//_displaying the results.
											//It is a (n,5) two-dimensional wave (matrix), where
											//_n is the number of the wave prefixes of the signals
											//_and calculation results. The columns are defined
											//_as:
											//  column 0:    Wave name prefix.
											//  column 1:    WaveList matching string.
											//  column 2:    Type discription.
											//  column 3:    Window title.
											//  column 4:    Window name prefix.
											//The rows in column 0 are just the prefix strings
											//_standing for the waves in the experiment, except "dbl",
											//_which refers no wave instanse but just for the import
											//_and export mode selection.
											//seealso: Wave Prefix
	rn[][1] = {"avg_*","amp*","tc*","rt*","hw*","tau1*","tau2*","a1*","a2*",minipref,epscpref,capref,"dbp*","rrp*","ppr*","ara*","nra*","arc*","nrc*"}

	for(i=0;i<n;i+=1)
		rn[i][0] = replacestring("*",rn[i][1],"")
	endfor

	rn[][2] = {"Average","Amplitude","Total Charge","Rise Time","Half Width","Tau1 (Fast)","Tau2 (Slow)","A1","A2","mEPSC","EPSC","Ica","Double Patch","RRP","Pair Pulse Ratio","Abs RRP (amp)","Norm RRP (amp)","Abs RRP (tc)","Norm RRP (tc)"}

	for(i=0;i<n;i+=1)
		rn[i][3] = "Plot_"+replacestring(" ",rn[i][2],"_")
	endfor

	for(i=1;i<9;i+=1)
		rn[i][3] = "Property_"+replacestring(" ",rn[i][2],"_")
	endfor
	for(i=15;i<19;i+=1)
		rn[i][3] = "Property_"+replacestring(" ",rn[i][2],"_")
	endfor

	variable vp
	for(i=0;i<n;i+=1)
		line = rn[i][3]
		vp = strsearch(line,"_",0)
		rn[i][4] = lowerstr(line[0,vp] + rn[i][0])
	endfor
	if(fmode)
		if(mdflag==0)
			ExperimentModified 0
		endif
	else
		string path = GetRecentPath(7,FALSE)
		string dfn = path+"defaultchmap"
		variable ref
		open/r/z=1 ref as dfn								//check existence of data file
		if(ref!=0)
			close ref
			ref = 0
		endif
		//	print v_flag
		if(!v_flag)
			loadwave/q/o/t dfn
			variable err = getrterror(1)
			if(err)
				//			print "err",err
			endif
		endif
	endif
	return ERROR_SUCCESS
End
Override Function CheckInit()
											//checks the status of procedure initialization.
											//details: This function checks the global variable isinit.
											//_It returns ERROR_SUCCESS only when isinit is created and set
											//_to be 1. Otherwise, it returns a nonzero error code.
	variable isin = NumVarOrDefault("root:isinit",0)
	variable r = ERROR_NOT_INITIALIZED
	if(isin==0)
		return r
	endif
	variable/g isinit
	r = ERROR_SUCCESS
	return r
End
Override Function GetVersion()
											//returns the version of this procedure.
	variable version = 4.211
	return version
End
Override Function UpdateVersion()
											//updates procedure version number of the currently running experiment.

											//call: GetVersion
	variable ipfversion
	ipfversion = GetVersion()
	if(waveexists($"expinfo")==0)
		return ERROR_INVALID_RES_WAVE
	endif
	wave exw = $"expinfo"
	exw[3] = ipfversion
	exw[5] = 0
	return ERROR_SUCCESS
End
Override Function CheckUpdate()
											//checks whether the experiment file needs update.
											//It will be automatically executed when the experiment file
											//_is opened or the procedure file is compiled.
											//call: InitReserve, CheckInit, GetVersion, UpdateExist
	variable igvck							//ignore version check
	variable cversion = GetVersion()

	if(CheckInit())
//		print "x"
		InitReserve(ALL,TRUE)
		return ERROR_SUCCESS
	endif
	igvck = 0
	variable oldversion
	if(waveexists($"expinfo"))
		wave exw = $"expinfo"
		oldversion = exw[3]
		igvck = exw[5]
	else
		oldversion = NumVarOrDefault("root:ipfversion",0)
	endif
	if(igvck)
		return ERROR_IGNORED
	endif
	if(round(1000*oldversion)==round(1000*cversion))
		return ERROR_NOT_NEEDED
	endif
//	print oldversion,cversion
	if(round(1000*oldversion)<round(1000*cversion))
		//	print oldversion - cversion
		string dstr = ""
		if(oldversion)
			dstr = "earlier version \"mepsc_v"+num2str(oldversion)+".ipf\".\rUpdate now?"
		else
			dstr = "unknown version.\rUpdate now?"
		endif
		doalert/t="Procedure version control" 1,"You may need to update the experiment file.\rThe experiment file is created or latest modified by " +dstr
		if(v_flag==1)
			UpdateExist(oldversion)
		else
			if(waveexists($"expinfo"))
				wave exw = $"expinfo"
				exw[5] = 1
			endif
		endif
	else
		doalert/t="Procedure version control" 0,"You may need to replace the procedure file.\rThe experiment file is created or latest modified by procedure of higher version \"mepsc_v"+num2str(oldversion)+".ipf\".\rSome errors may occur during procedure execution."
		if(waveexists($"expinfo"))
			wave exw = $"expinfo"
			exw[5] = 1
		endif
	endif
	return ERROR_SUCCESS
End
Override Function UpdateExist(ov)
											//updates experiment file created by procedure of old version.
											//details: It is used for providing the compatibilities of procedure
											//_to the experiment files created by earlier versions.
											//_The reserved waves, numeric and string variables will be updated and / or
											//_modified to fit new methods and procedure codes.
											//Some variables, strings list waves or data folders may be deleted. But NO
											//_original or extracted data wave will be affected.
											//call: CheckInit, GetVersion, UpdateVersion, SetConfig, SetChannel, SetDatatype, ScaledmEPSC, FitScaledmEPSC, StatsResult, GetAmplitude, RenameWave, CleanTemp, GetWaveList, KillWave, KillWindows
	variable ov								//oldVersion
											//is the parameter for explicitly choosing the old procedure version
											//_number. If you don't know what the exact number is, pass 0 and the
											//_procedure will check and set it according to data stucture characteristics.
	variable/g expconfig
	variable oldversion,olddatatype,updaten
//	variable/g ipfversion
	if(waveexists($"expinfo"))
		wave exw = $"expinfo"
		oldversion = exw[3]
	else
		oldversion = NumVarOrDefault("root:ipfversion",0)
	endif
//	print oldversion
	if((ov>0)&&(oldversion==0))
		oldversion = ov
	endif
//	print oldversion
//	print oldversion-getversion()
	if(round(oldversion*1000)==round(GetVersion()*1000))
		return ERROR_NOT_NEEDED
	endif

	variable i,miniwln,wi,oldmt,j,n
	string wl,wn,wnr
	if(CheckInit())
		return ERROR_NOT_INITIALIZED
	endif
	if(oldversion)
		string zstr = ""
		if(strsearch(num2str(oldversion),".",0)==-1)
			zstr = ".0"
		endif
		print "The experiment was created or latest modified by \"mepsc_v"+num2str(oldversion)+zstr+".ipf\"."
	endif
	olddatatype = 1

	if(oldversion==0)

		oldversion = 3.0
		prompt oldversion,"Former ipf Version:"
		prompt updaten,"Update Number:"
		prompt olddatatype,"Data Type:",popup,"mEPSC;eEPSC;Calcium"
		doprompt "Set Parameters: ",oldversion,updaten,olddatatype
	endif
	if(v_flag==1)
		print "User Cancelled."
		return ERROR_USER_CANCELED
	endif
	if(exists("datatype")==0)
		variable/g datatype
		datatype = olddatatype
	else
		variable/g datatype
	endif

	oldversion = oldversion + updaten/1000
	oldversion = round(oldversion*1000)
//	print oldversion
	if (oldversion<3000)
		print "Version "+num2str(oldversion)+" is too old to be updated."
		return ERROR_TOO_OLD_VERSION
	endif

	string/g postwl
	if(oldversion<3816)
		string/g indexwl
		postwl = indexwl
		killstrings indexwl

	endif
	if (oldversion<3550)			//no "mntime" / "mnname", used miniwl
		if(waveexists($"mntime")&&waveexists($"mnname"))

		else

			variable/g wln
			string/g minipref

			wl = wavelist(minipref,";","")
			miniwln = ItemsInList(wl)

			if(!waveexists($"mntime"))
				make/n=0 mntime
				oldmt = 0
			else
				redimension/n=(miniwln) $"mntime"
			endif

			if(!waveexists($"mnname"))
				make/n=0/t mnname
			else
				redimension/n=(miniwln) $"mnname"
			endif



			for (i=0;i<miniwln;i+=1)
				wn = StringFromList(i,wl,";")
				if(oldmt==0)
					insertpoints inf,1,mntime
					mntime[i] = 0.1*i
					insertpoints inf,1,mnname
					mnname[i] = wn
				endif

			endfor
			print "Miniature List Updated."
		endif
	endif

	if(oldversion<3703)
		string/g owpref

		string/g postwpref
		postwpref = "post*"
		variable/g wln

		for(i=0;i<wln;i+=1)
			wn = stringfromlist(i,postwl,";")
			wnr = postwpref[0,strlen(postwpref)-2]+num2str(i)
			if(waveexists($wn))
				rename $wn,$wnr
			endif
		endfor

		killstrings owpref

		postwl = wavelist(postwpref,";","")
		print "Indexed WaveList & Prefix Updated."
	endif
	if(oldversion<3704)
		expconfig = 0
		print "Experiment Config Updated."
	endif
	if(oldversion<3800)
		string/g epscpref = "epsc*"
		variable/g postpt,sf,orglength,wln
		mntime*=(postpt+1)/sf


		datatype = 1
		print "Data Type Updated."
	endif
	if (oldversion<3813)			//no "stimfiber", used stimf etc.
		if(waveexists($"stimf"))
			concatenate/o {waitt,stimf,stimn,pseudot,pseudob},stimfiber

		endif
		if(waveexists($"cainfo"))	//no "stimca", used cainfo
			rename $"cainfo" $"stimca"
		endif
	//	if(oldversion<round(GetVersion()*1000))
		SetConfig(TRUE)
	//	endif
	endif

	if (oldversion<3817)			//used mtime, not amp vs mntime
		if(waveexists($"mtime"))
			if(cmpstr(winlist("Time_Amp",";","Win:7"),"")!=0)
				wave/t mnname
				wave mntime
				RemoveFromGraph/w=$"Time_Amp" $"mtime"
				if(!waveexists($"amp"))
					string/g minipref
					miniwln = itemsinlist(wavelist(minipref,";",""))
					wave/t mnname = $"mnname"
					make/o/n=(miniwln) $"amp"
					wave amp = $"amp"
					for(wi=0;wi<miniwln;wi+=1)
						wn = mnname[wi]
						amp[wi]=GetAmplitude(wn)
					endfor
				endif


				if(waveexists($"amp"))
					wave amp = $"amp"
					amp = abs(amp)
					DoWindow/F $"Time_Amp"
					AppendToGraph amp vs mntime
					ModifyGraph mode=1
					setscale/i y,0,0,"A",amp
					setscale/i y,0,0,"s",mntime
					SetAxis left 0,*
					SetAxis bottom 0,*
				endif
			endif
			killwave("mtime",TRUE,TRUE)
		endif
	endif

	if(oldversion<3900)
		string/g minipref
		miniwln = itemsinlist(wavelist(minipref,";",""))
		if(waveexists($"mntime"))
			redimension/n=(miniwln) $"mntime"
		endif

		if(waveexists($"mnname"))
			redimension/n=(miniwln) $"mnname"
		endif
	endif
	if(oldversion<3901)
		if(waveexists($"tau1thres")!=0)
			wave tau1thres = $"tau1thres"
			wave tau1thresn = $"tau1thresn"
			wave/t tau1threswn = $"tau1threswn"

			make/o/n=(max(wavemax(tau1thresn),numpnts(tau1thresn)),numpnts(tau1thres)+1)/t grouptau1

			for(i=0;i<numpnts(tau1thres);i+=1)
				wave/t w = $tau1threswn[i]
				for(j=0;j<tau1thresn[i];j+=1)
					grouptau1[j][i] = w[j]
				endfor

				killwave(tau1threswn[i],TRUE,TRUE)
				grouptau1[i][dimsize(grouptau1,1)-1] = "Tau1_"+num2str(tau1thres[i-1])+"_"+num2str(tau1thres[i])
			endfor
			killwave(tau1threswn[i],TRUE,TRUE)
			grouptau1[0][dimsize(grouptau1,1)-1] = "Tau1_0_"+num2str(tau1thres[0])
			grouptau1[i][dimsize(grouptau1,1)-1] = "Tau1_"+num2str(tau1thres[i])+"_inf"
			killwave("tau1thres",TRUE,TRUE)
			killwave("tau1thresn",TRUE,TRUE)
			killwave("tau1threswn",TRUE,TRUE)
			killwindows("*tau1_*","",1)
		endif

	endif

	if(oldversion<3910)
		string/g prewpref,postwpref
		if(datatype==1)
			if(strlen(wavelist(prewpref,";",""))!=0)
				string/g prewl
				prewl = wavelist(prewpref,";","")
			endif
		endif
		if(datatype==3)
			string/g prewl,postwl
			postwl = wavelist(postwpref,";","")
			wln = itemsinlist(postwl)

			for(i=0;i<wln;i+=1)
				wn = stringfromlist(i,postwl,";")

				wnr = prewpref[0,strlen(prewpref)-2]+wn[strlen(postwpref)-1,inf]
				if(waveexists($wn))
					duplicate/o $wn $wnr
				endif
				killwave(wn,TRUE,TRUE)
			endfor
			prewl = wavelist(prewpref,";","")
		endif
	endif
	if(oldversion<3912)

		if(datatype==1)

			String/G plotwindow = "Plot_mEPSC"
			string/g scaledwindow = "Plot_Scaled_mEPSC"
			string/g scaledfitwindow = "Plot_Fit_Scaled_mEPSC"
			string/g plottimeamp = "Plot_Time_Amp"

			if(strlen(winlist("Time_Amp",";","WIN:7"))!=0)
				Dowindow/f $"Time_Amp"
				dowindow/c $lowerstr(plottimeamp)
				dowindow/t $plottimeamp,plottimeamp
			endif
			if(strlen(winlist("MiniPlot",";","WIN:7"))!=0)
				Dowindow/f $"MiniPlot"
				dowindow/c $lowerstr(plotwindow)
				dowindow/t $plotwindow,plotwindow
			endif
			if(strlen(winlist("MiniScale",";","WIN:7"))!=0)
				Dowindow/f $"MiniScale"
				dowindow/c $lowerstr(scaledwindow)
				dowindow/t $scaledwindow,scaledwindow
			endif
			if(strlen(winlist("MiniScaleFit",";","WIN:7"))!=0)
				Dowindow/f $"MiniScaleFit"
				dowindow/c $lowerstr(scaledfitwindow)
				dowindow/t $scaledfitwindow,scaledfitwindow
			endif



			wave/t mnname = $"mnname"
			n = numpnts(mnname)
			for(i=0;i<n;i+=1)
				wn = mnname[i]
				if(strsearch(wn,"-",0)>-1)
					wnr = replacestring("-",wn,"0")
					mnname[i] = wnr
					rename $wn,$wnr
					if(waveexists($"scaled_"+wn))
						rename $"scaled_"+wn,$"scaled_"+wnr
					endif
					if(waveexists($"fit_scaled_"+wn))
						rename $"fit_scaled_"+wn,$"fit_scaled_"+wnr
					endif

					string/g capref,minipref
					if(expconfig>1)
						wn = capref[0,strlen(capref)-2]+wn[strlen(minipref)-1,inf]
						wnr = replacestring("-",wn,"0")
						if(waveexists($wn))
							rename $wn,$wnr
						endif
					endif
				endif
			endfor
			rename plotwindow $"plotmini"
			rename scaledwindow $"plotsmini"
			rename scaledfitwindow $"plotfsmini"
		endif
	endif
	if(oldversion<3913)
		n = 15
		make/t/n=(n,4)/o rn
		string/g minipref,epscpref,capref
		rn[][1] = {"avg_*","amp*","tc*","rt*","hw*","tau1*","tau2*","a1*","a2*",minipref,epscpref,capref,"dbp*","rrp*","ppr*"}
		for(i=0;i<n;i+=1)
			rn[i][0] = replacestring("*",rn[i][1],"")
		endfor
		rn[][2] = {"Average","Amplitude","Total Charge","Rise Time","Half Width","Tau1 (Fast)","Tau2 (Slow)","A1","A2","mEPSC","EPSC","Ica","Double Patch","RRP","Pair Pulse Ratio"}
		for(i=0;i<n;i+=1)
			rn[i][3] = "Plot_"+replacestring(" ",rn[i][2],"_")
		endfor

		for(i=1;i<9;i+=1)
			rn[i][3] = "Property_"+replacestring(" ",rn[i][2],"_")
		endfor
		killstrings/z minipref,epscpref,capref

	endif
	if(oldversion<3915)
		if(datatype==1)
			wave mntime = $"mntime"
			wave/t mnname = $"mnname"
			wavestats/q/m=1 mntime

			if(v_npnts<numpnts(mntime))
				deletepoints v_npnts,inf, mntime
			endif
		endif
	endif
	if(oldversion<4001)
		if(datatype==3)
			wave/t rn = $"rn"
			RenameWave("Sub_"+rn[11][0],rn[11][0],1,3,1)
		endif
	endif
	string line = ""
	if(oldversion<4008)
		string/g fdpref


		string/g plotmini = "Plot_mEPSC"
		string/g plotsmini = "Plot_Scaled_mEPSC"
		string/g plotfsmini = "Plot_Fit_Scaled_mEPSC"
		string/g plottimeamp = "Plot_Time_Amp"
		string/g plotstats = "Plot_Statistics"

		make/t/o/n=5 $"rnw"
		wave/t rnw
		rnw = {plotmini,plotsmini,plotfsmini,plottimeamp,plotstats}
		killstrings fdpref
		variable/g fdn
		killvariables fdn
	endif
	if(oldversion<4009)
		if(exists("expsign")==0)
			variable/g pos
			rename pos,expsign
		endif
	endif
	if(exists("channel")==2)
		rename $"channel" $"axonch"
	endif
	if(exists("SF")==2)
		rename $"SF" $"sf"
	endif
	if(oldversion<4012)
		make/o/n=60 $"expinfo"
		wave exw = $"expinfo"
		variable/g usedefault,wln,dstats,ipfversion,isdefault,isdebug
		variable/g ampth,ampmax,prept,postpt,orglength,autosettrain,hekach,axonch,axongain
		ipfversion = oldversion
		exw[0] = usedefault
		exw[1] = wln
		exw[2] = dstats
		exw[3] = ipfversion
		exw[4] = isdebug
//		exw[5] = igvck
		exw[10] = ampth
		exw[11] = ampmax
		exw[12] = prept
		exw[13] = postpt
		exw[14] = orglength
//		exw[15] = isgmfin
//		exw[16] = autojudge
//		exw[17] = allowmipsc
		exw[20] = autosettrain
		exw[51] = hekach
		exw[52] = axonch
		exw[53] = axongain
		killvariables usedefault,wln,dstats,ipfversion,isdefault,isdebug
		killvariables ampth,ampmax,prept,postpt,orglength,autosettrain,hekach,axonch,axongain
	endif
//abort
//	print "!0",numpnts($"amp")
	if(oldversion<4100)
		string/g prewpref,postwpref
		wave/t rn
		n = numpnts(rn)/4
		insertpoints n,6,rn
		rn[n][1] = "ara*"
		rn[n+1][1] = "nra*"
		rn[n+2][1] = "arc*"
		rn[n+3][1] = "nrc*"
		rn[n+4][1] = prewpref
		rn[n+5][1] = postwpref
		for(i=n;i<n+6;i+=1)
			rn[i][0] = replacestring("*",rn[i][1],"")
		endfor
		rn[n][2] = "Abs RRP (amp)"
		rn[n+1][2] = "Norm RRP (amp)"
		rn[n+2][2] = "Abs RRP (tc)"
		rn[n+3][2] = "Norm RRP (tc)"
		rn[n+4][2] = "Pre Current"
		rn[n+5][2] = "Post Current"
		for(i=13;i<n+6;i+=1)
			rn[i][3] = "Plot_"+replacestring(" ",rn[i][2],"_")
		endfor
		for(i=n;i<n+4;i+=1)
			rn[i][3] = "Property_"+replacestring(" ",rn[i][2],"_")
		endfor
		make/t/o/n=(n+6) rnwinn

		variable vp
		for(i=0;i<n+6;i+=1)
			line = rn[i][3]
			vp = strsearch(line,"_",0)
			rnwinn[i] = lowerstr(line[0,vp] + rn[i][0])
		endfor
		concatenate {rnwinn},$"rn"
		killwave("rnwinn",TRUE,TRUE)
		for(i=9;i<12;i+=1)
			wl = wavelist(rn[i][1],";","")
			if(strlen(wl))
				n = itemsinlist(wl)
				if(cmpstr("A",waveunits($stringfromlist(0,wl),1),1))
					for(j=0;j<n;j+=1)
						setscale/p y,0,0,"A",$stringfromlist(j,wl)
					endfor
				endif
			endif
		endfor
		for(i=19;i<21;i+=1)
			wl = wavelist(rn[i][1],";","")
			if(strlen(wl))
				n = itemsinlist(wl)
				if(cmpstr("A",waveunits($stringfromlist(0,wl),1),1))
					for(j=0;j<n;j+=1)
						setscale/p y,0,0,"A",$stringfromlist(j,wl)
					endfor
				endif
			endif
		endfor


	endif
	if(oldversion<4104)
		wl = stringlist("*",";")
		wl = removefromlist("prewl;postwl;fn",wl,";",0)

		n = itemsinlist(wl)
		for(i=0;i<n;i+=1)
			killstrings/z $stringfromlist(i,wl)
		endfor
		wl = variablelist("*",";",4)
		wl = removefromlist("isinit;datatype;expsign;sf;expconfig;pta",wl,";",0)
		//print wl
		n = itemsinlist(wl)
		for(i=0;i<n;i+=1)
			killvariables/z $stringfromlist(i,wl)
		endfor
		print "Global numeric, string variables and waves updated."
	endif
	variable st = ticks
	if(oldversion<4108)
		if(waveexists($"mntime"))
	//	print "xx",numpnts($"mntime"),numpnts($"mnname"),numpnts($"amp")

			if((numpnts($"mntime"))!=(numpnts($"amp"))||((numpnts($"mnname"))!=(numpnts($"amp"))))
				redimension/n=(numpnts($"amp")) $"mntime",$"mnname"
			endif
		endif
						
		variable/g ncore = ThreadProcessorCount
		if(waveexists($"exhis"))
			redimension/n=(dimsize($"exhis",0),2) $"exhis"
		endif
//		if((numpnts($"mnname"))&&(numpnts($"mnname")<5000))
//			UniLength(rn[9][1])
//			print rn[9][2]+" length updated."
//		endif

		if(strlen(GetWaveList("*[property:5]",1,TRUE)))
			doalert/t="Update property result" 1,"Bad property results (e.g., negative tau1)\rwill be ignored now. Update results?"
			if(v_flag==1)
				StatsResult()
			endif
		endif
	endif

	if(oldversion<4200)
		if(waveexists($"mnname"))
			n = numpnts($"mnname")
//			print "!1",n
			if(waveexists($"amp"))
				n = numpnts($"amp")
			endif
//			print "2",n
			wave/t mnname,rnw
			variable hds,hdfs

			if(strlen(winlist(rnw[1],";","")))
				getwindow $rnw[1],hide
				hds = v_value
			else
				hds = 1
			endif
			if(strlen(winlist(rnw[2],";","")))
				getwindow $rnw[2],hide
				hdfs = v_value
			else
				hdfs = 1
			endif
			if(n<500)
				killwindows(rnw[1]+";"+rnw[2],"",1)

				for(i=0;i<n;i+=1)
					wn = mnname[i]
					ScaledmEPSC(wn,0)
					FitScaledmEPSC(wn,0)
				endfor
								
				killwave("*scaled_mini*",FALSE,TRUE)
				CleanTemp(TRUE)

				ScaledmEPSC("*",0)
				FitScaledmEPSC("*",0)

				if(hds)
					dowindow/hide=1 $rnw[1]
				endif
				if(hdfs)
					dowindow/hide=1 $rnw[2]
				endif
				//		print "Scaled- and fitted-scaled- mEPSC waves updated."
			endif
		endif
	endif

	if(oldversion<4202)
		wave exw = $"expinfo"
		if(exw[10]<1e-9)
			exw[10] = round(exw[10]*1e12)
		endif
	endif
	if(oldversion<4203)
		killwave("expfd",TRUE,TRUE)
		string/g prewl,postwl
		wave/t rn
		rn[11][2] = "Ica"
		if(strlen(prewl+postwl))

			wave/t rn
			wave exw = $"expinfo"
			make/o/n=0/t chorg,w_chidx,adlist,gvlist,dtlist

			variable/g expconfig,datatype
			variable hch,ach,achg
			string hchstr,achstr,achgstr
			hch = exw[51]

			ach = exw[52]
			achg = exw[53]
//			print "xxx",hch,ach,achg
			switch(expconfig)

				case 0:
					if(hch)
						hchstr = num2str(hch)
					else
						hchstr = "1"
					endif
					redimension/n=1 chorg,w_chidx,gvlist
					w_chidx = hchstr
					gvlist = {"1"}
					redimension/n=(1,2) adlist,dtlist
					adlist = {{"Imon2 (HEKA)"},{"1"}}
					dtlist = {{(rn[datatype+8][2])},{"A"}}
					if(datatype<3)
						chorg = postwl
					else
						chorg = prewl
					endif
					break
				case 1:
					if(ach)
						achstr = num2str(ach)
					else
						achstr = "1"
					endif
					if(achg)
						achgstr = num2str(achg)
					else
						if(datatype==1)
							achgstr = "20"
						else
							achgstr = "0.5"
						endif
					endif

					redimension/n=1 chorg,w_chidx,gvlist
					w_chidx = achstr
					gvlist = {achgstr}
					redimension/n=(1,2) adlist,dtlist
					adlist = {{"AD-0 (Axon)"},{achgstr}}
					dtlist = {{rn[datatype+8][2]},{"A"}}
					if(datatype<3)
						chorg = postwl
					else
						chorg = prewl
					endif
					break
				case 2:
					if(hch)
						hchstr = num2str(hch)
					else
						hchstr = "1"
					endif
					if(ach)
						achstr = num2str(ach)
					else
						achstr = "1"
					endif
					if(achg)
						achgstr = num2str(achg)
					else
						if(datatype==1)
							achgstr = "20"
						else
							achgstr = "0.5"
						endif
					endif
					redimension/n=2 chorg,w_chidx,gvlist
					w_chidx = {hchstr,achstr}
					gvlist = {"1",achgstr}
					redimension/n=(2,2) adlist,dtlist
					adlist = {{"Imon2 (HEKA)","AD-0 (Axon)"},{"1",achgstr}}
					dtlist = {{rn[datatype+8][2],rn[11][2]},{"A","A"}}
					chorg = {postwl,prewl}
					break
				case 3:
					if(hch)
						hchstr = num2str(hch)
					else
						hchstr = "1"
					endif
					if(ach)
						achstr = num2str(ach)
					else
						achstr = "1"
					endif
					if(achg)
						achgstr = num2str(achg)
					else
						if(datatype==1)
							achgstr = "20"
						else
							achgstr = "0.5"
						endif
					endif
					redimension/n=2 chorg,w_chidx,gvlist
					w_chidx = {hchstr,achstr}
					gvlist = {"1",achgstr}
					redimension/n=(2,2) adlist,dtlist
					adlist = {{"Imon2 (HEKA)","AD-0 (Axon)"},{"1",achgstr}}
					dtlist = {{rn[11][2],rn[datatype+8][2]},{"A","A"}}
					chorg = {prewl,postwl}
					break
				case 4:
				default:
			endswitch
			redimension/n=40 $"expinfo"
			duplicate/r=[][0] adlist,adlist0
			duplicate/r=[][1] adlist,adlist1
			duplicate/r=[][0] dtlist,dtlist0
			duplicate/r=[][1] dtlist,dtlist1
			redimension/n=(numpnts(w_chidx)) adlist0,adlist1,dtlist0,dtlist1
			sort w_chidx,w_chidx,adlist0,adlist1,dtlist0,dtlist1,gvlist,chorg
			concatenate/kill/O {adlist0,adlist1},$"adlist"
			concatenate/kill/O {dtlist0,dtlist1},$"dtlist"
			SetChannel("",TRUE)
			print "Channel map updated."
		endif
		SetDataType()
		killvariables/z $"expconfig"
	endif
	if(oldversion<4209)
		wave exw = $"expinfo"
		if(exw[11]<1e-9)
			exw[11] = round(exw[11]*1e12)
		endif
		exw[2] = TRUE
		killstrings/z prewl,postwl
		wave/t rn
		redimension/n=(19,dimsize(rn,1)) rn
	endif
	wave exw = $"expinfo"

	CleanTemp(!exw[4])
	UpdateVersion()
	return ERROR_SUCCESS
End
Override Function/S MenuItem(mstr)
										//is used to build the dynamic menu items.
										//call: CheckInit, GetVersion, GetResWaveList, GetWaveList
										//seealso: Menu, User-Defined Menus
	string mstr
	variable dt = NumVarOrDefault("root:datatype",0)
	string dstr = "("
	if(strlen(GetResWaveList(1))==0)
		mstr = dstr + mstr
		return mstr
	endif
	wave/t rn
	variable dis = 0
	strswitch(mstr)
		case "&Average/F9":
			if(itemsinlist(removefromlist(GetResWaveList(1),wavelist("*",";","")))==0)
				dis = 1
			endif
			break
		case "&Properties/F10":
			if((!(dt&3))||(itemsinlist(GetWaveList(rn[9][1]+";"+rn[10][1]+";",TRUE,FALSE))==0))
				dis = 1
			endif
			break
		case "Co&rrelation":
		case "Show &Event Frequency":
			if(itemsinlist(GetWaveList("[property:]",TRUE,FALSE))==0)
				dis = 1
			endif
			break
		case "&Frequency":
		case "&Classify mEPSC Group":
		case "Show Plot &Layout":
		case "Show Property &Bands":
		case "Recheck mEPSC":
		case "&Kill mEPSC":
		case "Align to Peak":
		case "Align to Diff":
			if((!(dt&1))||(itemsinlist(wavelist(rn[9][1],";",""))==0))
				dis=1
			endif
			break
		case "Kill &Orphan Waves":
			//	print dt
			if((dt&5)!=5)
				//		print dt
				dis=1
			endif
			break
		case "&Resume Judge/F8":
			if((!(dt&1))||(itemsinlist(GetWaveList("expinfo;rchmap;",TRUE,TRUE))==0))
				dis = 1
			endif
			break
		case "&Pair Pulse Ratio":
		case "&RRP Size":
		case "&Steady Phase Analyses":
		case "Set &Train Config":
		case "&Export Property Stats":
			if((!(dt&2))||(itemsinlist(wavelist(rn[10][1],";",""))==0))
				dis=1
			endif
			break
		case "Get &Calcium Current":
			if((!(dt&4))||(itemsinlist(wavelist(rn[11][1],";",""))==0))
				dis=1
			endif
			break
		case "&Display Waves":
		case "&Concatenate Waves":
		case "&Smooth Waves":
		case "&Rename Waves":
		case "&Kill Waves":
		case "C&olorize Waves":
		case "&Normalize":
		case "&Adjust Axon Gain":
			if(itemsinlist(removefromlist(GetResWaveList(1),wavelist("*",";","")))==0)
				dis = 1
			endif
			break
		case "Show &File Information":
			if(itemsinlist(GetWaveList("rn;rnw;expinfo",TRUE,TRUE))<3)
				dis = 1
			endif
			break
		case "&Update Old File":
			if(CheckInit())
				dis = 1
			else
				if(waveexists($"expinfo"))
					wave exw = $"expinfo"
					variable ov = exw[3]
					if(round(ov*1000)==round(Getversion()*1000))
						dis = 1
					endif
				else
					nvar/z iov = $"ipfversion"
					if(nvar_exists(iov))
						if(round(iov*1000)==round(Getversion()*1000))
							dis = 1
						endif
					endif
				endif
			endif
	endswitch
	if(dis)
		mstr = dstr+mstr
	endif
	return mstr
End
Override Function LoadIgorText(dfn)
											//loads Igor text waves (.ITX) exported from HEKA PatchMaster,
											//_containing original source data waves.
											//details: If the dfn is a partial name (contains no path information), a list
											//_of recently used paths will be checked. If dfn is still not a valid file name,
											//_an open dialog will show up.
											//After loading original text waves, the sequential data discrimination
											//_will be proceeded, depending on the data type and experiment
											//_configurations.
											//call: Initial, LoadITX, GetmEPSC, GeteEPSC, GetCaCurrent, CleanUp
	string dfn								//dataFileName
											//is used to locate the exact file which contains the text waves
											//_acquired and exported by recording software.
											//This parameter is designed for unattendant batch tasks intiated by command
											//_line and programs. It only works when referring to a valid file.
											//_If it is not a full path neme, the file named as dfn in the same
											//_folder with the saved experiment, recently opened ITX, the opened user
											//_procedure file, user document, or desktop will be searched for use.



	CleanUp()
	Initial()

	String/G fn								//is the full-length name of the Igor text file (.ITX) which
											//_contains the original data waves.
	variable wln							//is the number of indexed waves.

//	String postwl							//is the list string which contains indexed postsynaptic
											//_current wave names.
//	String prewl							//is the list string which contains indexed presynaptic
											//_current wave names.
	//	String/G orgwl				//original full-length wave name list

	//	if(expconfig<2)			//Single Patch



	variable/g datatype

	if(waveexists($"expinfo")==0)
		return ERROR_INVALID_WAVE
	endif
	LoadITX(1,dfn)

	wave exw = $"expinfo"
	wln = exw[1]
	if(wln>0)
		if(datatype&1)
			GetmEPSC(0)
			print "mEPSC extraction complete. "+num2str(numpnts($"mntime"))+" mEPSC added."
		else
			if(datatype&4)
				GetCaCurrent()
			else
				if(datatype&2)
					GeteEPSC(-1,0)
				endif
			endif
			//	Average("EPSC")
		endif
	else
		print "No original sweep loaded. Check the channel settings."
//		isinit = 0
	endif
//	else						//Double Patch
//		LoadITX(3,dfn)
//		SetChannel()
////		Print "Processing original data..."
//		SaveOrgName(wl)
//		RenameOrg(wl)
//		wave exw = $"expinfo"
//		wln = exw[1]
//		if(wln>0)
//			if(datatype==1)
//				GetmEPSC(0)
//			else
//				if(datatype ==3)
//					GetCaCurrent()
//				endif
//			endif
//		else
//			print "No original sweep loaded. Check the channel settings."
//			isinit = 0
//		endif
//	endif
	return ERROR_SUCCESS
End
Override Function SetConfig(cfgmode)
											//sets experiment configuration.
											//details: SetConfig will be called when you execute a data input
											//_operation. Procedure need to know how the original waves are
											//_organized (e.g., the channel-data type map) or what the current
											//_experiment is for.
											//The input dialog can be skipped only when cfgmode is set to FALSE. In
											//_this case, the function will set the global switches and flags
											//_as default values, according to either the saved default wave or the codes in
											//_procedure file. The default wave can be set and saved if you pass
											//_TRUE to cfgmode and choose "Save as default" before clicking "Continue"
											//_in the prompt dialog.
											//call: GetVersion, KillWave, GetRecentPath
	variable cfgmode						//configMode
											//is the flag used to determine whether the dialog for experiment
											//_configuration parameters assignment will be shown or not.
											//  TRUE    Prompt a dialog for user to input and select.
											//  FALSE    Use default values from saved default waves or procedure codes.
	variable usedefault = TRUE			//is a switch to specify whether the default values should
											//_be used as the arguments. The values are:
											//  0    Disabled, Ask user to set.
											//  1    Enabled, Use default settings.
											//The default value is 0.

	variable/G expsign = 0					//is the sign of original wave baseline values. It is used to check
											//_the holding potentials used during electrophysiological experiments. The values could be:
											//  -1    All Negative.
											//  0    Detect by program.
											//  1    All Positive.
											//The default value is 0.
	variable/G sf = 20000					//is the sampling frequency of acquired original waves.
											//_The default value is 20000 (Hz).
	variable expconfig = 0					//is used to show the experiment configuration.
											//  0    Single patch,  AD: Imon2.
											//  1    Single patch,  AD: AD-0.
											//  2    Double patch,  AD: Imon2 (post) & AD-0 (pre).
											//  3    Double patch,  AD: Imon2 (pre) & AD-0 (post).
											//  4    Double patch,  AD: Vmon / Imon2 & AD-0 (both post).
											//The AD channels Imon2 and Vmon which PatchMaster used are
											//_built-in channels for HEKA EPC10 amplifier current and
											//_voltage data. The AD-0 (or maybe other AD input channel)
											//_is for Axon Axopatch 200B. The default value of expconfig
											//_is 0 (HEKA single).
	variable datatype = 1					//is the glabal flag to show the type of the original data.
											//_The values are:
											//  1     Miniature EPSC (mEPSC) data.
											//  2     Fiber stimulation evoked EPSC (eEPSC) data.
											//  4     Presynaptic calcium current (calcium) data.
											//The default value is 1 (mEPSC).

	variable/g ncore = ThreadProcessorCount	//is the CPU core number. In multithread tasks, it is used to
												//_set the threads number.

	variable dstats = TRUE 				//is a switch for displaying detailed graphs and tables
											//_after the properties calculation automatically or not.
											//_Assign it with a nonzero number will turn the switch on
											//_and vice versa.
											//_The default value is 1.
	variable ipfversion					//contains the version of this procedure.
											//_The format is main version (1 digit) + sub version (1 digit)
											//_+ update number (2 digit), e.g., 3.916.
	ipfversion = GetVersion()
	variable isdebug = FALSE				//is a switch which alters the mode for debugging.
	variable isdefault						//is a flag to show whether the default settings has been used
											//_or not. Not functional, reserved for future version. The
											//_values are:
											//  0    Modified.
											//  1    Default.
	variable ampth = 5						//is the amplitude minimum threshold for mEPSC extraction.
											//_Only signals with amplitudes over this value will be
											//_gathered for further calculation.
											//_The default value is 5[pA].
	variable ampmax = 100					//is the amplitude maximum threshold for automatical
											//_filter process during mEPSC judgement. The signals, of
											//_which the amplitudes are bigger than this value, will be
											//_identified as artifacts or noises rather than mEPSC.
											//_The default value is 1e-10 (100pA).
	variable prept = 2/1000*sf			//is the length of the time window (in points) before mEPSC
											//_onset point for mEPSC signal extraction. So it's also the
											//_default length of mEPSC baseline. The default value equals
											//_to 2e-3 * sf (points within 2ms).
	variable postpt = 5/1000*sf-1			//is the length of the time window (in points) after mEPSC
											//_onset point for mEPSC signal extraction. It is the point
											//_number of mEPSC onset to the tail. The default value equals
											//_to 5e-3 * sf (points within 5ms).
	variable orglength						//is the length (in points) of the original waves.
											//_It is valid only if the original traces are of same length
											//_and is currently used when datatype is 1 (mEPSC data).
	variable autosettrain = TRUE 			//is a switch to control whether to set fiber stimulation
											//_pseudo trace parameters only once per cell or not.
											//_It is useful only when the datatype is set to 2 (eEPSC).
											//_The default value is 1.
	variable isupdate,isshow
	string func = getrtstackinfo(2)
	if(cmpstr(func,"updateexist"))
		isupdate = FALSE
	else
		isupdate = TRUE
	endif

//	abort
	string path = GetRecentPath(7,FALSE)+"defaultexpinfo"

	variable ref
	if(waveexists($"expinfo")==0)			//set default if setting not exist
		open/r/z ref as path
		if(ref)
			close ref
			ref = 0
		endif
		if(!v_flag)
			loadwave/o/t path
			if(waveexists($"def_expinfo"))
				wave def_expinfo
				if(numpnts(def_expinfo))
					duplicate/o def_expinfo $"expinfo"
				endif
			endif
			KillWave("def_expinfo",TRUE,TRUE)
		endif
	endif
	if(waveexists($"expinfo")==0)			//cannot find default settings, use coded settings

		make/o/n=40 expinfo				//is the wave which contains numeric values of
											//_experiment settings.
											//One of the reasons to build and use this wave
											//_is to minimize the utilization of global numeric
											//_variables and protect the settings. Many global
											//_variables in earlier versions have been integrated
											//_and encapsulated into this wave.
											//It is a 40-element vector where the rows are defined as:
											//  Row 0x:    Procedure primary settings.
											//  Row 1x:    mEPSC functions settings.
											//  Row 2x:    eEPSC functions settings.
											//  Row 3x:    Ica functions settings.


		expinfo[0] = cfgmode
		expinfo[2] = dstats
		expinfo[3] = ipfversion
		expinfo[4] = isdebug
		expinfo[10] = ampth
		expinfo[11] = ampmax
		expinfo[12] = prept
		expinfo[13] = postpt
		expinfo[20] = autosettrain

		isshow = 1
	endif
	wave exw = $"expinfo"

//	cfgmode = exw[0]
//	dstats = exw[2]
//	ampth = exw[10]
//	ampmax = exw[11]
	prept = exw[12]
	postpt = exw[13]
//	autosettrain = exw[20]

	//	print prept,postpt,ampth,ampmax

	if((cfgmode))//	||(isshow))					//prompt input dialog
		variable newsf,newpretime,newposttime,newconfig,exppos
		if(sf==20000)
			newsf = 1
		else
			newsf = 2
		endif

		newpretime = (prept) *1000/SF
		newposttime = (postpt+1) *1000/SF
		exppos = expsign
		newconfig = expconfig+1
		variable newampth =ampth
		variable newampmax = ampmax
		variable newdatatype= datatype
		if(isupdate)

			Prompt newdatatype,"Data type:",popup,"mEPSC;eEPSC;Calcium"
			Prompt newsf,"Sample frequency [Hz]:",popup,"20000 Hz;50000 Hz"
			Prompt newpretime, "Time before T0 [ms]:"
			Prompt newposttime, "Time after T0 [ms]:"
			Prompt exppos,"Experiment holding potential:"popup, "-80mV;+80mV"
			Prompt newconfig,"Experiment configuration:",popup,"SINGLE PATCH: HEKA;SINGLE PATCH: Axon;DOUBLE PATCH: HEKA POST + Axon PRE;DOUBLE PATCH: HEKA PRE + Axon POST;DOUBLE PATCH: HEKA & Axon POST"
			Prompt newampth,"Amplitude threshold [pA]:"
			Prompt newampmax, "Maximum amplitude [pA]:"
			if(datatype==1)
				Doprompt "Set parameters:",newdatatype,newsf,newpretime,newposttime,exppos,newconfig,newampth,newampmax//,newhekapref
			else
				Doprompt "Set parameters:",newdatatype,newsf,exppos,newconfig//,newhekapref
			endif


			if(v_flag==1)						//user continued
				return ERROR_USER_CANCELED
			endif
			if((datatype!=1)&&(newdatatype==1))		//show mepsc settings if datatype changes from !1 to 1

				Doprompt "Set parameters:",newsf,newpretime,newposttime,newampth
				if(v_flag==1)				//return if cancelled
					//		print "User cancelled configuration. Use default settings."
					return ERROR_USER_CANCELED
				endif
			endif
			if(exppos==1)
				expsign = -1
			else
				expsign = 1
			endif
			if(newsf==1)
				sf = 20000
			endif
			if(newsf==2)
				sf = 50000
			endif


			prept=newpretime/1000*SF
			postpt=newposttime/1000*SF-1

			//			hekapref = newhekapref
			expconfig = newconfig-1
			nvar/z excfg = $"expconfig"
			if(nvar_exists(excfg))
				datatype=newdatatype
				excfg = expconfig
			endif
//			if (expsign==1)
//				ampmax = 300
//			else
//				ampmax = 100
//			endif
			ampmax = newampmax
			exw[10] = ampth
			exw[11] = ampmax
			exw[12] = prept
			exw[13] = postpt
			//			isdefault = 0
			//			wave/t rn
			//			rn[0][1] = hekapref
			print "Configuration updated."
		else
			Prompt newsf,"Sample frequency [Hz]:",popup,"20000 Hz;50000 Hz"
			Prompt newpretime, "Time before T0 [ms]:"
			Prompt newposttime, "Time after T0 [ms]:"
			Prompt exppos,"Experiment holding potential:"popup, "-80mV;+80mV"
			Prompt newampth,"Amplitude threshold [pA]:"
			Prompt newampmax, "Maximum amplitude [pA]:"
			variable svdef = 2
			Prompt svdef,"Save as default:",popup,"Yes;No"
			if(datatype&1)
				Doprompt "Set parameters:",newpretime,newposttime,exppos,newampth,newampmax,svdef
			else
				Doprompt "Set parameters:",newsf,exppos,svdef
			endif
			if(v_flag)
				return ERROR_USER_CANCELED
			endif
			if(datatype&1)
				prept=newpretime/1000*SF
				postpt=newposttime/1000*SF-1
				exw[10] = newampth
				exw[11] = newampmax
				exw[12] = prept
				exw[13] = postpt
			endif
			if(newsf==1)
				sf = 20000
			endif
			if(newsf==2)
				sf = 50000
			endif
			if(exppos==1)
				expsign = -1
			else
				expsign = 1
			endif
			if(svdef==1)
				save/o/t expinfo as path
			endif
		endif
	else
		//	print "Use default settings."
	endif
	return ERROR_SUCCESS
End
Override Function ShowExpInfo()

											//prints the experiment configuration, original source and extracted
											//_wave information, procedure status into the history window.
											//call: GetVersion
	variable/g expsign,datatype,sf
	variable expconfig
	if(waveexists($"expinfo")==0)
		return ERROR_INVALID_RES_WAVE
	endif
	wave exw = $"expinfo"
	variable usedefault = exw[0]
	variable wln = exw[1]
	variable dstats = exw[2]
	variable ipfversion = exw[3]
	variable autosettrain = exw[20]

	if((ipfversion<1)&&(wln!=0))
		print "No information index has been found. Update existed data first."
		return ERROR_NO_TARGET_WAVE
	endif
	string udc,dt,ast,ds,ec,ep
//	if(usedefault==1)
//		udc = "Yes"
//	else
//		udc = "No"
//	endif
	if(dstats==1)
		ds = "Yes"
	else
		ds = "No"
	endif
	if(datatype==2)
		ast = ", Judge pseudo Only Once per Cell = "
		if(autosettrain ==1)
			ast = ast + "Yes"
		else
			ast = ast + "No"
		endif

	else
		ast = ""
	endif
	switch(expsign)
	case 1:
		ep = "Positive"
		break
	case -1:
		ep = "Negative"
		break
	default:
		ep = "Automatically Detect by Programme"
	endswitch
	wave/t rn
	variable i,n
	n = ceil(log(datatype)/log(2))+1

	dt = ""
	for(i=0;i<n;i+=1)
		if(datatype&(2^i))
			dt += rn[i+9][2]+","
		endif
	endfor
	if(strlen(dt)==0)
		dt = "Unknown,"
	endif

	string ipfver = num2str(ipfversion)
	if(cmpstr(ipfver,"0")==0)
		ipfver = "Unknown Version"
	else
		ipfver = "Version "+ipfver
	endif
	ec = "SINGLE PATCH: HEKA;SINGLE PATCH: Axon;DOUBLE PATCH: HEKA POST + Axon PRE;DOUBLE PATCH: HEKA PRE + Axon POST;DOUBLE PATCH: HEKA & Axon POST"
	ec = stringfromlist(expconfig,ec,";")
	string info = ""
	info = "Experiment Settings:\r"
	info += "    Automatically Display Statistical Result = "+ds+ast+"\r"
	info += "    Data Type = "+ dt +" Sample Frequency = "+num2str(sf) + " Hz"//, Exp Config = "+ ec+", Holding Potential = "+ ep + "\r"//+", "
	print info
	variable pren,postn,mn,can,epscn,ach,hch,again
	string postwl,prewl
	string/g fn

	string dsn = ""

//	print n
	for(i=0;i<n;i+=1)
		if(datatype&(2^i))
			mn = itemsinlist(wavelist(rn[i+9][1],";",""))
			dsn += "    " +rn[i+9][2] +" waves: n = "+num2str(mn)+"\r"
		endif
	endfor
	if(strlen(dsn)==0)
		dsn = "    No extracted wave.\r"
	endif


	info = "Data Information:\r"
	if(strlen(fn)!=0)
		info += "    Original data loaded from file \"" + fn + "\"\r"
	endif

	info += dsn
	print info
	info = "Procedure Information:\r"
	info += "    Created or latest modified by "+ ipfver + "\r"
	info += "    Currently running procedure version = " + num2str(GetVersion()) + "\r"
	print info
	return ERROR_SUCCESS
End
Override Function LoadITX(ldmode,dfn)
											//loads waves from Igor text file.

											//details: If the dfn is just a partial file name with no path information,
											//_the files named dfn in recently used paths will be checked. If dfn is
											//_still not a valid file name, it shows open dialog.
											//seealso: Import
											//call: SetChannel, RecHistory, GetRecentPath, SaveRecentPath
	variable ldmode						//loadMode
											//is used to determine which kind of data will be loaded.
											//_The sequential renaming process will be affect by the
											//_argument passed. The values could be:
											//  1    Load original source waves from ITX file.
											//  2    Load saved waves for data analyzing.
	string dfn								//dataFileName
											//is used to specify the file to open. If it is a partial
											//_name, the file in the same folder with the saved experiment
											//_file, recently opened ITX, or the user procedure will be tried.

	variable ref
	string wn
	ref=strsearch(dfn,":",0)
	if(waveexists($"expinfo"))
		wave exw = $"expinfo"
		variable isdebug = exw[4]
		variable issdb
		if(isdebug==0)
			execute/z/q "modifybrowser close"
			issdb = v_flag
		endif
	else
		execute/z/q "modifybrowser close"
	endif
	//	print ref,dfn
	if(ref==-1)											//partial name
		dfn =  GetRecentPath(ALL,TRUE) + dfn
		pathinfo/s $"recent"
		//		dfn = replacestring(":",dfn,"\\")
		//		dfn = replacestring("\\",dfn,":\\",0,1)
	endif
	//		print dfn
	//	print "Start loading ITX file..."

	open/r/z=1 ref as dfn								//check existence of data file
	if(ref!=0)
		close ref
		ref = 0
		//		if(getrterror(1))
		//			print "xxx"
		//		endif
	endif
	string wl = wavelist("*",";","")
	if ((strlen(dfn)==0)||(v_flag!=0))				//show open dialog if file not exist
		//		SetBackground ShowLoadProgress()
		//		CtrlBackground dialogsOK=1,period=60,start
		//		loadwave/q/o/t
		String ff = "IGOR Text Files (*.itx,*.awav):.itx,.awav;"
		open/d/r/z=2/f=ff ref as dfn
		//		print v_flag
		//		print ref
		//		print s_filename
		//close/a
		//load igor text file
		if((v_flag!=0)||(strlen(s_filename)==0))		//cancel clicked
			print "User cancelled loading. Procedure abort."

			//			CtrlBackground stop
			execute/p/q/z "abort"
			return ERROR_USER_ABORTED

		endif
		dfn = s_filename								//get full name

		dfn = replacestring(":",dfn,"\\")
		dfn = replacestring("\\",dfn,":\\",0,1)		//format name
	endif
	if(ref!=0)
		//	print "zzz",ref
		close ref
		ref = 0
		//		if(getrterror(1))
		//			print "zzz"
		//		endif
	endif
	//	print i
	//file dfn exists
	//		SetBackground ShowLoadProgress()
	//		CtrlBackground dialogsOK=1,period=60,start
	variable st = ticks
	variable fileSize = 0,memsize=0
	open/r/z=2 ref as dfn
	if(ref)
		FStatus ref
		if(v_flag)
			fileSize = v_logeof
		endif
		close ref
		ref = 0
		//		if(getrterror(1))
		//			print "yyy"
		//		endif
	else
		//		print "Open file error."
	endif
	setdatafolder root:
//	if(0)
//		//	killdatafolder root:load
//		newdatafolder root:load
//		variable pid = ThreadGroupCreate(1)				//create thread group
//		//	abort
//		memsize = NumberByKey("FREEMEM",IgorInfo(0))
//		ThreadStart pid, 0, LoadWaveMT(dfn)				//launch thread
//		ThreadGroupPutDF pid,root:$"load"					//put data folder to hold new waves
//		do
//			DFREF dfr= ThreadGroupGetDFR(pid,100)
//
//			if ( DatafolderRefStatus(dfr) == 0 )
//				//		print "filesize:",filesize,"memsize",NumberByKey("FREEMEM",IgorInfo(0))
//				//		ValDisplay prgs,value= _NUM:1,win=ProgressBar
//				//		DoUpdate /W=ProgressBar
//				//		print "abcde"
//			else
//				///		print "xxx"
//				MoveDataFolder dfr,root:					//convert free data folder
//				break
//			endif
//		while(1)
//
//		if(ThreadGroupRelease(pid))
//			print "Some threads are still running. Force quit is needed. Thread Group ID:"+num2str(pid)+"."
//		endif
//		//	endif
//		setdatafolder root:
//		MoveWaves("*","root:load","root:")
//		if(strlen(FolderList("","load")))
//			killdatafolder $"load"
//		endif
//	else
	loadwave/q/o/t dfn
//	endif
	setdatafolder root:
	string/g fn
	fn = dfn
	SaveRecentPath(fn)

	//	CtrlBackground stop
	wl = RemoveFromList(wl, wavelist("*",";",""),";",0)
	print "Loading data from file \""+fn+"\" complete. "+num2str((ticks-st)/60) +"s used."
	make/o/n=2/t tmpexhis = {num2str(ldmode),fn}
	RecHistory()

	if(ldmode==1)
		//	SetChannel()
		SetChannel(wl,TRUE)
		//		Print "Processing original data..."
		//	SaveOrgName(wl)
		//	RenameOrg(wl)

	endif
	if(waveexists($"expinfo"))
		if(isdebug==0)
			if(!issdb)
				execute/z/q "createbrowser"
			endif
		endif
	endif
//	if((strsearch(fn,"Axon",0,2)!=-1)&&(expconfig==0))	//correct single patch
															//configuration according to file name
//		expconfig=1
//	endif

//	if((strsearch(fn,"HEKA",0,2)!=-1)&&(expconfig==1))
//		expconfig=0
//	endif



						//format wave name


//	case 2:		//load saved mepsc,OBSOLETED
//		miniwl = wavelist(minipref,";","")
//		variable miniwln
//		miniwln = ItemsInList(miniwl)
//		if(miniwln>0)
//			wn = StringFromList(0,miniwl,";")
//			wavestats/m=1/q $wn
//			SetSign(sign(v_avg))
//			for(i = 0; i < miniwln; i+=1)
//				wn = StringFromList(i,miniwl,";")
//				PlotmEPSC(wn)
//			endfor
//		endif
//		print miniwln,"mEPSC Wave(s) Loaded."
//	break
	return ERROR_SUCCESS
End
Override Function SetChannel(wl,scmode)
											//sets or shows channel configurations of originally loaded waves.
											//call: SetDatatype, ButtonListener, KeyListener, KillWave
	string wl								//waveList
											//is the list of waves which are to be used. If it is an empty string,
											//_the procedure will try to build a list according to HEKA PatchMaster
											//_exported wave name format.
	variable scmode						//setMode
											//is for specifying the function will run as setting channel map or just
											//_showing existing values. The values for this parameter should be:
											//TRUE    Set channel configuration map.
											//FALSE    Show saved channel configuration map.
	wl = replacestring(",",wl,";")
	if(strlen(wl)==0)
		wl = greplist(wavelist("*",";","TEXT:0"),"(_\d+){4,}$|((_\d+){3,}(_\d+|p\d+){1})$")
	endif
	string chl = ""
	string unkl = ""
	string dl = greplist(wl,"_\d+$")
	unkl = removefromlist(dl,wl)
	wl = dl
	//print dl
	string chidx = ""
	variable i,n
	string ptitle,isshow
	if(scmode)			//set channel config
		if((waveexists($"chorg")==0)||(waveexists($"w_chidx")==0))
			make/t/o/n=0 chorg

			i=0
			do
				chl = greplist(wl,"_"+num2str(i)+"$")
				if(strlen(chl))
					chidx+="Channel "+num2str(i)+";"
					n = numpnts(chorg)
					redimension/n=(n+1) chorg
					chorg[n] = chl
					wl = removefromlist(chl,wl)
				endif
				if(strlen(wl)==0)
					break
				endif
				//		print strlen(wl)
				i+=1
			while(1)

			if(strlen(unkl))
				chidx+="Channel X;"
				n = numpnts(chorg)
				redimension/n=(n+1) chorg
				chorg[n] = unkl
			endif
			make/o/t/n=(n+1) ingv = "1"
			make/o/t/n=(n+1) incb = "1"
			make/o/t/n=(n+1) indt = "Choose..."
			//	print chidx
			//	print unkl
		else
			wave/t chorg
			wave/t w_chidx
			wave/t dtlist,adlist
			n = numpnts(w_chidx)
			make/o/t/n=(n) indt
			make/o/t/n=(n) ingv
			for(i=0;i<n;i+=1)
				chidx+="Channel "+w_chidx[i]+";"
				indt[i] = dtlist[i][0]
				ingv[i] = adlist[i][1]
			endfor
		endif
		n = itemsinlist(chidx)
		if(!n)
			return ERROR_ZERO_OBJECT_LENGTH
		endif
		if(waveexists($"rchmap"))
			duplicate/o rchmap,tmprchmap
		endif
		make/o/n=(n,8)/t rchmap = ""			//stores the channel configuration of original waves.
												//rchmap is a (n,8) two-dimensional text wave (matrix),
												//_where n is the number of total channels recognized by
												//_procedure according to suffix(es) of originally loaded
												//_waves. The columns are defined as:
												//  column 0:    Original channel number.
												//  column 1:    AD channel name / prefix.
												//  column 2:    Data type.
												//  column 3:    AD gain value.
												//  column 4:    Wave unit.
												//  column 5:    Is external input or not.
												//  column 6:    Original wave name list.
												//  column 7:    Formatted wave name list.

												//NOTE: If the names of loaded waves don't match with the
												//_standard PatchMaster export naming format, the channel
												//_number will be set as "X".

		isshow = "0"
		ptitle = "Set "
	else				//show channel config
		if(waveexists($"rchmap")==0)
			return ERROR_INVALID_RES_WAVE
		endif
		wave/t rchmap
		n = dimsize(rchmap,0)
		duplicate/r=[][1,3]/o rchmap,$"adlist"
		wave/t adlist
		deletepoints/m=1 1,1,$"adlist"
		duplicate/r=[][2,3] rchmap,$"dtlist"
		wave/t dtlist
	//	dtlist[][1]=""
		duplicate/r=[][3] rchmap,$"gvlist"
		duplicate/r=[][2] rchmap,$"indt"
		duplicate/r=[][3] rchmap,$"ingv"
		redimension/n=(n) $"gvlist",$"indt",$"ingv"
		redimension/n=(n,2) $"adlist",$"dtlist"
		make/o/t/n=(n) indt = ""
		for(i=0;i<n;i+=1)
			chidx += "Channel "+rchmap[i][0]+";"
			adlist[i][0] = replacestring("src_",adlist[i][0],"")
			adlist[i][0] = upperstr(adlist[i][0])
			adlist[i][0] = replacestring("MON",adlist[i][0],"mon")
			if(grepstring(adlist[i][0],"^AD\d+_$"))
				adlist[i][0] = (adlist[i][0])[0,1]+"-"+(adlist[i][0])[2,inf]+"(Axon)"
			else
				if(grepstring(adlist[i][0],"(^\wmon\d*_$)|(^(Cm|Gs|Gm)$)"))
					adlist[i][0] = adlist[i][0] + "(HEKA)"
				endif
			endif
			adlist[i][0] = replacestring("_",adlist[i][0]," ")
//			print adlist
//			print dtlist
			if(grepstring(dtlist[i][0],"^\w{3,4}AP$"))
	//		print "xxx",			dtlist[i][0]
				dtlist[i][0] = (dtlist[i][0])[0,strlen(dtlist[i][0])-3]+"-AP"
		//		print dtlist[i][0]
			else
				if(grepstring(dtlist[i][0],"^Stim\w$"))
					dtlist[i][0] = (dtlist[i][0])[0,3]+"["+(dtlist[i][0])[4]+"]"
				endif
			endif

			indt[i] = dtlist[i][0]
		endfor
		duplicate/o rchmap,tmprchmap
		isshow = "2"
		ptitle = "Show "
	endif
//	print "xxx"
//abort
	if(waveexists($"adlist")==0)
		make/o/n=(9,2)/t adlist = {{"Imon2 (HEKA)","Vmon (HEKA)","AD-0 (Axon)","AD-1 (Axon)","AD-2 (Axon)","AD-3 (Axon)","AD-4 (Axon)","Cm (HEKA)","Gm (HEKA)","Gs (HEKA)","Other"},{"1","1","Set","Set","Set","Set","Set","1","1","1","Set"}}
	endif
	if(waveexists($"gvlist")==0)
		make/o/n=(9)/t gvlist = {"1","2","5","10","20","0.5","0.2","0.1","Other"}
	endif
	if(waveexists($"dtlist")==0)
		make/o/n=(10,2)/t dtlist = {{"mEPSC","Stim[V]","Ica","Pre-AP","Post-AP","Stim[A]","EPSC","Cm","Gm","Gs","Other"},{"A","V","A","V","V","A","A","F","F","S","-"}}
	endif
	wave/t adlist,gvlist,dtlist

	NewPanel /N=setchpanel/W=(320,120,800,320+n*30)/FLT=2/K=2 as ptitle+"channel configuration"

	SetActiveSubwindow _endfloat_

	display/W=(21000,21000,21000,21000)/K=1/n=lockpanel as ""
	SetActiveSubwindow lockpanel

	setwindow lockpanel,hook(listener)=KeyListener

//	setwindow setchpanel,hook(tmphook)=KeyListener

	for(i=0;i<n;i+=1)

		groupbox gbtop,pos={10,25},size={460,40+n*30},frame=2,win=setchpanel
		titlebox gbtb0,pos={55,34},title="Enabled",frame=0,win=setchpanel
		titlebox gbtb1,pos={161,34},title="Source AD",frame=0,win=setchpanel
		titlebox gbtb2,pos={290,34},title="Data type",frame=0,win=setchpanel
		titlebox gbtb3,pos={408,34},title="Gain",frame=0,win=setchpanel
		execute/q/z "checkbox cbch"+num2str(i)+",pos={30,"+num2str(60+i*30)+"},size={100,20},title=\" "+stringfromlist(i,chidx)+"\",fstyle=1,value=1,win=setchpanel,userdata(istr)=\""+stringfromlist(i,chidx)+"\",disable="+isshow//+stringfromlist(i,chidx)+"\",frame=0"
//		execute/q/z "titlebox tbch"+num2str(i)+",pos={55,"+num2str(60+i*30)+"},size={100,20},fstyle=1,title=\""+stringfromlist(i,chidx)+"\",frame=0,win=setchpanel,userdata(istr)=\""+stringfromlist(i,chidx)+"\",disable="+isshow
		execute/q/z "button bach"+num2str(i)+",pos={140,"+num2str(56+i*30)+"},size={100,20},title=adlist["+num2str(i)+"][0],proc=ButtonListener,win=setchpanel,userdata(istr)=adlist["+num2str(i)+"][0],disable="+isshow
		execute/q/z "button bdch"+num2str(i)+",pos={270,"+num2str(56+i*30)+"},size={100,20},title=indt["+num2str(i)+"],proc=ButtonListener,win=setchpanel,userdata(istr)=dtlist["+num2str(i)+"][0],disable="+isshow
		execute/q/z "button bgch"+num2str(i)+",pos={400,"+num2str(56+i*30)+"},size={40,20},title=adlist["+num2str(i)+"][1],proc=ButtonListener,win=setchpanel,userdata(istr)=ingv["+num2str(i)+"],disable="+isshow
	endfor
	//	button bchoose,fColor=(65535,65535,65535),appearance={default,all}//,valuecolor=
	if(scmode)
		checkbox cbsave,pos={270,75+n*30},size={100,20},title=" Save settings as default.",value=0,win=setchpanel
//		titlebox tbsave,pos={295,75+n*30},size={100,20},title="Save settings as default.",frame=0,win=setchpanel
	endif

	groupbox gbbottom,pos={10,125+n*30},size={460,50},frame=2,win=setchpanel
	if(scmode)
	button bsccancel,pos={80,(140+i*30)},size={70,20},title="Cancel",proc=ButtonListener,win=setchpanel
	button bscdefault,pos={205,(140+i*30)},size={70,20},title="Default",proc=ButtonListener,win=setchpanel
	button bscyes,pos={330,(140+i*30)},size={70,20},title="Done",proc=ButtonListener,win=setchpanel
	else
	button bsccancel,pos={80,(140+i*30)},size={70,20},title="Cancel",proc=ButtonListener,win=setchpanel
	button bscdefault,pos={205,(140+i*30)},size={70,20},title="Default",proc=ButtonListener,win=setchpanel,disable=2
	button bscyes,pos={330,(140+i*30)},size={70,20},title="Done",proc=ButtonListener,win=setchpanel,disable=2

	endif

	PauseForUser setchpanel,lockpanel

//	PauseForUser setchpanel,kwframe
	wave setchquit
	if(setchquit[0])
		if(waveexists($"tmprchmap"))
			duplicate/o $"tmprchmap",$"rchmap"
		else
			killwave("rchmap",TRUE,TRUE)
		endif
//	print waveexists($"adlist")
//	if(waveexists($"adlist"))
//		print dimsize($"adlist",0),dimsize($"adlist",1)
//	endif

		killwave("chorg;adlist;dtlist;gvlist;w_chidx;setchquit;indt;ingv;incb;tmprchmap;def_chmap;",TRUE,TRUE)
//			print waveexists($"adlist")
//	if(waveexists($"adlist"))
//		print dimsize($"adlist",0),dimsize($"adlist",1)
//	endif

		return ERROR_USER_CANCELED
	endif
//	print "byes clicked"
//	killwaves setchquit
	string regex = ""
//	print chidx
	for(i=0;i<n;i+=1)
		if(strlen(rchmap[i][0]))
//			regex = stringfromlist(i,chidx)[8,inf]		//save regex
//			if(cmpstr(regex,"X"))
//				regex = "_"+regex+"$"
//			else
//				regex = "_\\D$"
//			endif
//			rchmap[i][0] = chorg[i]
			rchmap[i][0] = stringfromlist(i,chidx)[8,inf]
			rchmap[i][6] = chorg[i]						//save original names
		endif

	endfor
	for(i=0;i<n;i+=1)
		if(strlen(rchmap[i][0])==0)
			deletepoints i,1, rchmap
			i-=1
			n-=1
		endif
	endfor
	n = dimsize(rchmap,0)
//	variable vp
	for(i=0;i<n;i+=1)
		findvalue/text=rchmap[i][2]/txop=2 dtlist
	//	print v_value-dimsize(dtlist,0)
		rchmap[i][4] = dtlist[v_value][1]
		if(stringmatch(rchmap[i][1],"*(HEKA)"))
			rchmap[i][5] = ""
		else
			rchmap[i][5] = "1"
		endif
		rchmap[i][1] = replacestring(" (Axon)",rchmap[i][1],"")
		rchmap[i][1] = replacestring(" (HEKA)",rchmap[i][1],"")
		rchmap[i][1] = replacestring("-",rchmap[i][1],"")
		rchmap[i][1] = replacestring(" ",rchmap[i][1],"")
		rchmap[i][2] = replacestring("-",rchmap[i][2],"")
		rchmap[i][2] = replacestring("[",rchmap[i][2],"")
		rchmap[i][2] = replacestring("]",rchmap[i][2],"")
		rchmap[i][1] = lowerstr("Src_"+rchmap[i][1] + "_")
	endfor

	variable j,m
	variable isadin,isyu,isxu,delx,lex,isload,gain
	if(waveexists($"w_chidx")==0)
		isload=1
	endif
	m = dimsize(rchmap,0)
	string wn,pref,wnr
	string yunit,xunit
	for(j=0;j<m;j+=1)
		wl = rchmap[j][6]
		pref = rchmap[j][1]
		KillWave(pref+"*",TRUE,TRUE)
		n = itemsinlist(wl)
		isadin = strlen(rchmap[j][5])
		gain = str2num(rchmap[j][3])
		if(n)
			yunit = rchmap[j][4]
			xunit = "s"
			wn = stringfromlist(0,wl)
			isyu = cmpstr(waveunits($wn,0),yunit)
			isxu = cmpstr(waveunits($wn,0),xunit)
			if(isxu)
				delx = deltax($wn)
				lex = leftx($wn)
			endif
		endif
		for(i=0;i<n;i+=1)
			wn = stringfromlist(i,wl)
			wnr = pref+num2str(i)
			rename $wn, $wnr
			wave w = $wnr
		//	print isadin,isload
			if(isadin*isload)
		//		print "xx",wnr
				w *= (1e-9/gain)
			endif
			if(isyu)
				setscale d,0,0,yunit,w
			endif
			if(isxu)
				setscale/p x,lex,delx,xunit,w
			endif
		endfor
		rchmap[j][7] = wavelist(pref+"*",";","")
	endfor

	killwave("chorg;adlist;dtlist;gvlist;w_chidx;setchquit;indt;ingv;incb;tmprchmap;def_chmap;",TRUE,TRUE)


//	variable/g datatype
//	wave/t rn
	SetDatatype()
	wave exw = $"expinfo"
	if(waveexists(exw))
		exw[1] = itemsinlist(rchmap[0][6])
	endif
	return ERROR_SUCCESS
End
Override Function SetDatatype()
											//is used to set datatype according to source waves.
											//call: GetWaveList
											//seealso: Global Numeric Variables
	variable/g datatype					//is the glabal variable which shows the type of the original data.
											//_It is a bitwise variable defined as:
											//  Bit 0:     Miniature EPSC (mEPSC) data.
											//  Bit 1:     Fiber stimulation evoked EPSC (eEPSC) data.
											//  Bit 2:     Presynaptic calcium current (Ica) data.
											//NOTE: Bit 0 and bit 1 are not allowed to be set simultaneously
											//_In other words, do NOT make an experiment file containing both
											//_mEPSC and eEPSC source waves. Otherwise the pre-post signal
											//_time coupling may return unexpected results.
											//Seealso: Setting Bit Parameters, Bitwise and Logical Operators
	variable sdmode = 1

	wave/t rn
	variable i,n
	string dtlist = ""			//loaded wave data type list
	string dl = ""				//defined data type list
	if(waveexists($"rchmap")==0)
		sdmode = 0
	else
		wave/t rchmap
		n = dimsize(rchmap,0)
		if(n==0)
			sdmode=0
		endif
	endif

	string dstr = ""
	variable vp
	if(sdmode)
		for(i=9;i<12;i+=1)
			dl += rn[i][2]+";"
		endfor
		for(i=0;i<n;i+=1)
			dtlist += rchmap[i][2]+";"
		endfor
		datatype = 0
//			print dtlist
//			print dl

		n = itemsinlist(dl)
		for(i=0;i<n;i+=1)
			dstr = stringfromlist(i,dl)
			vp = WhichListItem(dstr,dtlist,";",0,0)
//			print i,vp
			if(vp!=-1)
				datatype=datatype|(2^(i))
				dtlist = removefromlist(dstr,dtlist,";",0)
			endif
		endfor
	else
		for(i=9;i<12;i+=1)
			dl += rn[i][0]+";"
		endfor

		n = itemsinlist(dl)
		for(i=0;i<n;i+=1)
			dstr = stringfromlist(i,dl)
			vp = itemsinlist(getwavelist(dstr+"\d+(_\d+)*",TRUE,FALSE))
			if(vp)
				datatype=datatype|(2^(i))
			endif
		endfor
	endif
	return ERROR_SUCCESS
End
Override Function ButtonListener(btact) : ButtonControl
											//listens and handles user mouse events on button controls.
											//call: ActionListener
											//seealso: KeyListener
	STRUCT WMButtonAction &btact


	if(btact.eventCode==1)

		if (btact.eventMod & 0x01)
			string cn = btact.ctrlName,scn = ""
			scn = cn
			if(grepstring(scn,"^\S{4}\d+$"))
				scn = scn[0,3]
			endif
			variable i,n
			if(waveexists($"rchmap")==0)
				return ERROR_INVALID_RES_WAVE
			endif
			wave/t rchmap
			strswitch (scn)
				case "bscyes":
					ActionListener(cn,"setchpanel")
					break
				case "bscdefault":
					ActionListener(cn,"setchpanel")
					break
				case "bsccancel":
					ActionListener(cn,"setchpanel")
					break
				case "bscinyes":
					ActionListener(cn,"setchinput")
					break
				case "bscincancel":
					ActionListener(cn,"setchinput")
					break
				case "bjmyes":
				case "bjmno":
				case "bjmquit":
				case "bjmkill":
				case "bjmmulti":
				case "bjmmtyes":
					ActionListener(cn,"judgewindow")
					break
				case "bstpseudo":
					dowindow $"cursorwindow"
					if(v_flag)
						killwindow cursorwindow
					endif
					break
				case "bach":
				case "bdch":
				case "bgch":
					wave/t adlist,dtlist,gvlist
					string al=""
					string dl = ""
					string gl = ""

					n=dimsize(adlist,0)
					for(i=0;i<n;i+=1)
						al += adlist[i]+";"
					endfor
					n=dimsize(dtlist,0)
					for(i=0;i<n;i+=1)
						dl += dtlist[i][0]+";"
					endfor
					n = numpnts(gvlist)
					for(i=0;i<n;i+=1)
						gl += gvlist[i]+";"
					endfor
					String pl = "",txtl
					strswitch (cn[0,3])
					case "bach":
						pl = al
						txtl = "AD name:"
						break
					case "bdch":
						i = str2num(cn[4,inf])
						if(strlen(rchmap[i][1]))
							if(grepstring(rchmap[i][1],"^(Cm|Gm|Gs) \(HEKA\)$"))
								rchmap[i][2] = (rchmap[i][1])[0,1]
								return ERROR_NOT_ALLOWED
							endif
						else
							if(grepstring(getuserdata("setchpanel","bach"+cn[4,inf],"istr"),"^(Cm|Gm|Gs) \(HEKA\)$"))
								rchmap[i][2] = (rchmap[i][1])[0,1]
								return ERROR_NOT_ALLOWED
							endif
						endif
						pl = dl
						txtl = "Data type:"
						break
					case "bgch":
						wave/t rchmap
					//	string adn = "bach"+cn[4,inf]
						i = str2num(cn[4,inf])
						if(strlen(rchmap[i][1]))
							if(stringmatch(rchmap[i][1],"*(HEKA)"))
								rchmap[i][3] = getuserdata("setchpanel",cn,"istr")
								return ERROR_NOT_ALLOWED
							endif
						else
							if(stringmatch(getuserdata("setchpanel","bach"+cn[4,inf],"istr"),"*(HEKA)"))
								rchmap[i][3] = getuserdata("setchpanel",cn,"istr")
								return ERROR_NOT_ALLOWED
							endif
						endif
						pl = gl
						txtl = "Gain value:"
						break
					default:
					endswitch
					controlinfo/w=setchpanel $cn

					PopupContextualMenu/c=(V_left,V_top+V_Height) pl
					if(strlen(S_selection))
						if(cmpstr(S_selection,"other"))
							button $cn,title = S_selection,win=setchpanel
							i = str2num(cn[4,inf])
							strswitch (cn[0,3])
							case "bach":
								rchmap[i][1] = s_selection
								if(stringmatch(s_selection,"*(HEKA)"))
									button $"bgch"+num2str(i),title = "1",win=setchpanel
									rchmap[i][3] = "1"
									if(grepstring(s_selection,"^(Cm|Gm|Gs) \(HEKA\)$"))
										button $"bdch"+num2str(i),title = s_selection[0,1],win=setchpanel
										rchmap[i][2] = s_selection[0,1]
									endif
								else
									button $"bgch"+num2str(i),title = "Set",win=setchpanel
									rchmap[i][3] = ""
								endif
								break
							case "bdch":
								rchmap[i][2] = s_selection
								break
							case "bgch":
								rchmap[i][3] = s_selection
								break
							default:
							endswitch

						else
							if(cmpstr(txtl,"Gain value:"))
								i = 1
							else
								i = 0
							endif
							NewPanel/N=setchinput/W=(420,180,660,320+i*40)/FLT=2/K=2 as "Input " +txtl
							SetActiveSubwindow _endfloat_
							ModifyPanel/w=setchinput,noEdit=1

							titlebox tbtx,pos={30,42},size={70,20},title = txtl,win=setchinput,frame=0

							setvariable othertext,pos={110,40},size={90,20},bodywidth=90,title="",value=_str:"",win=setchinput//,frame=0,labelback=(65535,65535,65535)
							if(i)
							if(cmpstr(txtl,"AD name:"))
								titlebox tbug,pos={30,82},size={70,20},title = "Signal unit:",win=setchinput,frame=0
								setvariable otherunit,pos={110,80},size={90,20},bodywidth=90,title="",value=_str:"",win=setchinput//,frame=0,labelback=(65535,65535,65535)
							else
								titlebox tbug,pos={30,82},size={70,20},title = "Gain value:",win=setchinput,frame=0
								setvariable othergain,pos={110,80},size={90,20},bodywidth=90,title="",value=_str:"",win=setchinput//,frame=0,labelback=(65535,65535,65535)
							endif
							else
							endif
							button bscinyes,pos={30,80+i*40},size={70,20},title="Countinue",userdata(selbn)=cn,win=setchinput,proc=ButtonListener
							button bscincancel,pos={140,80+i*40},size={70,20},title="Cancel",win=setchinput,proc=ButtonListener

							PauseForUser setchinput,setchpanel

						endif
					endif
					break
				default:
			endswitch
		endif
	endif
	return ERROR_SUCCESS
End
Override Function KeyListener(src)
										//listens and handles user keyboard events.
										//details: NOTE: You may sometimes find that the keystroke
										//_events don't work. It is caused by the target window (majorly
										//_panel) losing focus. In this case, you nned to use the
										//_button instead.
										//call: ActionListener
										//seealso: ButtonListener
	STRUCT WMWinHookStruct &src
	if(cmpstr(src.winname,"lockpanel")==0)
//	print s.eventcode
		switch(src.eventCode)
			case 11:					// Keyboard event
				switch (src.keycode)
					case 13://enter
						dowindow $"setchinput"
						if(v_flag)
							ActionListener("bscinyes","setchinput")
						else
							dowindow $"setchpanel"
							if(v_flag)
								ActionListener("bscyes","setchpanel")
							else
								dowindow $"judgewindow"
								if(v_flag)
								else
								endif
							endif
						endif
						break
					case 27:		//esc
						dowindow $"setchinput"
						if(v_flag)
							ActionListener("bscincancel","setchinput")
						else
							dowindow $"setchpanel"
							if(v_flag)
								ActionListener("bsccancel","setchpanel")
							endif
						endif
						break
					case 32:		//spacebar,D,d
					case 68:
					case 100:
						dowindow $"setchinput"
						if(v_flag)
						else
							dowindow $"setchpanel"
							if(v_flag)
								ActionListener("bscdefault","setchpanel")
							endif
						endif
						break
					case 83:		//S,s
					case 115:
						dowindow $"setchinput"
						if(v_flag)
						else
							dowindow $"setchpanel"
							if(v_flag)
								ActionListener("cbsave","setchpanel")
							endif
						endif
						break
					case 89:
					case 121://Y,y
						dowindow $"setchinput"
						if(v_flag)
						else
							dowindow $"setchpanel"
							if(v_flag)
							else
								dowindow $"judgewindow"
								if(v_flag)
//								print "xxxx"
									ActionListener("bjmyes","judgewindow")
								endif
							endif
						endif
						break
					case 78:
					case 110://N,n
						dowindow $"setchinput"
						if(v_flag)
						else
							dowindow $"setchpanel"
							if(v_flag)
							else
								dowindow $"judgewindow"
								if(v_flag)
									ActionListener("bjmno","judgewindow")
								endif
							endif
						endif
						break
					case 80:
					case 112://P,p
						dowindow $"setchinput"
						if(v_flag)
						else
							dowindow $"setchpanel"
							if(v_flag)
							else
								dowindow $"judgewindow"
								if(v_flag)
									ActionListener("bjmmulti","judgewindow")
								endif
							endif
						endif
						break
					case 81:
					case 113://Q,q
						dowindow $"setchinput"
						if(v_flag)
						else
							dowindow $"setchpanel"
							if(v_flag)
							else
								dowindow $"judgewindow"
								if(v_flag)
									ActionListener("bjmquit","judgewindow")
								endif
							endif
						endif
						break
					case 82:
					case 114://R,r
						dowindow $"setchinput"
						if(v_flag)
						else
							dowindow $"setchpanel"
							if(v_flag)
							else
								dowindow $"judgewindow"
								if(v_flag)
									ActionListener("bjmkill","judgewindow")
								endif
							endif
						endif
						break
				endswitch
				break
		endswitch
	else
//		switch(s.eventCode)
//		case 4:
//			if( s.mouseLoc.v > ((s.winRect.top+s.winRect.bottom)/2) )
//				s.doSetCursor= 1
//				s.cursorCode= round(27*(s.mouseLoc.h - s.winRect.left)/ (s.winRect.right - s.winRect.left))
//			//	rval= 1			// we have taken over this event
//			endif
//			break
//			getwindow
//		endswitch
//	print s_cursorainfo
//		print s.cursorCode
//		print s.cursorName
//		print s.winname
//cursor
	endif
	return ERROR_SUCCESS
End
Override Function ActionListener(evt,winn)
											//is the listener function to deal with user actions.


											//call: InitReserve, AddmEPSC, HighLight, ScaledmEPSC, FitScaledmEPSC, ShowAverage, Frequency, CleanTemp, KillmEPSC, KillWindows, RecHistory, GetRecentPath
	string evt								//eventSource
											//is used to get the name of control which triggered the event.
	string winn								//winName
											//is the name of window where the event source is on.
	ControlInfo/W=$winn $evt
											//details: JudgemEPSC returns user operation of mEPSC judgement.
											//According to the button which the user clicked ("Yes"
											//_or "No"), this function executes in the opposite ways. If the
											//_user comfirmed the shown temperal wave as the mEPSC, it will be
											//_renamed according to the format mEPSC prefix + index. If the
											//_"No" button was clicked, it will cancel the modification, kill
											//_the prompted waves and informs the ScanOrg function to skip
											//_a period of time and find the next possible signal.
											//_makes the mEPSC waves and registers them.

											//QuitmEPSC handles user operations for terminating mEPSC judgement process.
											//When the user clicks the "Stop" button in the operation panal
											//_for mEPSC judgement, it will stop the scanning of original data waves
											//_and all other running functions. The windows used to display waves for
											//_judging will be closed and the color and line size of latest added mEPSC
											//_will be modified to normal.
											//All temporary waves for signal extaction are killed. The global
											//_flag ismepsc is set to -9.

											//MultiPeakmEPSC returns user operation of multiple peak mEPSC identification.
											//When the user clicks the "Multiple Peak" button on the panel,
											//_this function catches the operation. It widens the time window used
											//_for the judgement. Two cursors will be available for user to label
											//_the signals. When doing this, place the cursors onto the wave at the
											//_peaks of the signals. No matter the sequence that which cursor is
											//_earlier in time, the extracted mEPSCs will be right in temporal order.
											//Be sure of using the cursor A and B. Once you clicked the multiple-peak
											//_button, the function will continuously check the cursors on the graph
											//_and will not go forward unless both two has been placed. Because the
											//_baseline of the waves dicriminated by this method are shorter than
											//_the set prept in most cases, the properties of those mEPSCs may be
											//_slightly affected due to the base line value estimation.
											//seealso: Cursor

	if(v_disable)
		return ERROR_DISABLED
	endif
	dowindow $winn
	if(v_flag==0)
		return ERROR_INVALID_WINDOW
	endif
	variable i,n,pt,px,prept,postpt
	if(waveexists($"rchmap")==0)
		return ERROR_INVALID_RES_WAVE
	endif
	wave/t rchmap
	//		print evt
	string 	tg = "tmpmg",tgc = "tmpcag",lastwn,wn,wnc,wix,nx,minipref
	strswitch (evt)

		case "bscyes":
			for(i=0;;i+=1)
				//	cn =
				ControlInfo/W=setchpanel $"cbch"+num2str(i)
				if(v_flag!=2)
					break
				else
					if(v_value==1)
						rchmap[i][0] = "1"
						if(strlen(rchmap[i][1])==0)
							rchmap[i][1] = getuserdata("setchpanel","bach"+num2str(i),"istr")
							Button $"bach"+num2str(i) title = rchmap[i][1],win = setchpanel
						endif
						if(strlen(rchmap[i][2])==0)
							rchmap[i][2] = getuserdata("setchpanel","bdch"+num2str(i),"istr")
							Button $"bdch"+num2str(i) title = rchmap[i][2],win = setchpanel
						endif
						if(strlen(rchmap[i][3])==0)
							rchmap[i][3] = getuserdata("setchpanel","bgch"+num2str(i),"istr")
							Button $"bgch"+num2str(i) title = rchmap[i][3],win = setchpanel
						endif
					else
						rchmap[i][0] = ""
					endif
				endif
			endfor
			wave/t rn
			findvalue/text=(rn[9][2])/txop=2 rchmap
			if(v_value!=-1)
				findvalue/text=(rn[10][2])/txop=2 rchmap
				if(v_value!=-1)
					DoAlert/t="Data type conflict",0,"\"mEPSC\" and \"EPSC\" cannot be loaded\rsimultaneously from a single data file!"
					return ERROR_INVALID_CTRL_TEXT
				endif
			endif
			ControlInfo/W=setchpanel $"cbsave"
			if(v_value)
				duplicate/o rchmap,$"def_chmap"
				wave/t dchmap = $"def_chmap"
				n = dimsize(dchmap,0)
				for(i=0;i<n;i+=1)
					dchmap[i][4] = getuserdata("setchpanel","cbch"+num2str(i),"istr")
				endfor
				redimension/n=(n,5) dchmap
				string dfn = GetRecentPath(7,FALSE)+"defaultchmap"
				save/t/o dchmap as dfn
			endif

			killwindow setchpanel
			make/o/n=1 setchquit=0
			//abort
			break
		case "bscdefault":
			variable edv
			InitReserve(ALL,FALSE)
			if(waveexists($"def_chmap"))
				edv = 1
				wave/t dchmap = $"def_chmap"
			endif
			wave/t adlist,dtlist,ingv
			for(i=0;;i+=1)
				//	cn = "ch"
				ControlInfo/W=setchpanel $"bdch"+num2str(i)
				if(v_flag!=1)
					break
				else
					if(edv)
						//	titlebox $"tbch"+num2str(i),title = dchmap[i][4],win=setchpanel
						checkbox $"cbch"+num2str(i),value=str2num(dchmap[i][0]),title = dchmap[i][4],win=setchpanel
						rchmap[i][0] = dchmap[i][0]
						button $"bach"+num2str(i),title = dchmap[i][1],win=setchpanel
						rchmap[i][1] = dchmap[i][1]
						button $"bdch"+num2str(i),title = dchmap[i][2],win=setchpanel
						rchmap[i][2] = dchmap[i][2]
						button $"bgch"+num2str(i),title = dchmap[i][3],win=setchpanel
						rchmap[i][3] = dchmap[i][3]
					else
						button $"bach"+num2str(i),title = adlist[i][0],win=setchpanel
						rchmap[i][1] = adlist[i][0]
						button $"bdch"+num2str(i),title = dtlist[i][0],win=setchpanel
						rchmap[i][2] = dtlist[i][0]
						button $"bgch"+num2str(i),title = ingv[i],win=setchpanel
						rchmap[i][3] = ingv[i]
					endif
				endif
			endfor
			//
			break
		case "bsccancel":

			if((waveexists($"chorg")==0)||(waveexists($"w_chidx")==0))
				killwindow setchpanel
				make/o/n=1 setchquit=-1
			else
				doalert/t="" 1,"Abort experiment updating?"
				if(v_flag==1)
					killwindow setchpanel
					make/o/n=1 setchquit=-1
				endif
			endif
			break
		case "cbsave":
			controlinfo/w=setchpanel $evt
			if(v_flag!=2)
				return ERROR_INVALID_CONTROL
			endif
			checkbox cbsave,value=1-v_value,win=setchpanel
			//	modifycontrol cbsave,win=setscpanel
			break
		case "bscinyes":

			string selbn = getuserdata("setchinput","bscinyes","selbn")
			controlinfo/w=setchinput othertext
			if(strlen(S_Value)==0)
				//	print "!!!",s_value
				return ERROR_INVALID_CTRL_TEXT
			else
				if(stringmatch(selbn,"bgch*"))
					if((str2num(S_Value)<=0)||(grepstring(s_value,"^\D")))
						return ERROR_INVALID_CTRL_TEXT
					endif
				else
					if(grepstring(s_value,"^(\W|\d)"))
						return ERROR_INVALID_CTRL_TEXT
					else
						if(stringmatch(selbn,"bach*"))
							controlinfo/w=setchinput othergain
							//		print "?",s_value,str2num(S_Value)
							if((str2num(S_Value)<=0)||(grepstring(s_value,"^\D")))
								//		print "????"
								return ERROR_INVALID_CTRL_TEXT
							endif
						else
							if(stringmatch(selbn,"bdch*"))
								controlinfo/w=setchinput otherunit
								//			print "?",s_value
								if(grepstring(S_Value,"^(\W|_)"))
									//			print "????"
									return ERROR_INVALID_CTRL_TEXT
								endif
							endif
						endif
					endif
				endif
			endif
			controlinfo/w=setchinput othertext
			if(stringmatch(selbn,"bgch*"))
				s_value = num2str(str2num(s_value))
			endif
			button $selbn,title = S_Value,win=setchpanel


			i = str2num(selbn[4,inf])
			if(stringmatch(selbn,"bach*"))
				rchmap[i][1] = S_Value
				wave/t adlist
				n = dimsize(adlist,0)
				//	redimension/n=(n+1),adlist
				insertpoints n-1,1,adlist
				adlist[n-1] = s_value
				controlinfo/w=setchinput othergain
				adlist[n-1][1] = s_value
			endif
			if(stringmatch(selbn,"bdch*"))
				rchmap[i][2] = S_Value
				wave/t dtlist
				n = dimsize(dtlist,0)
				//						redimension/n=(n+1,2),dtlist
				insertpoints n-1,1,dtlist
				dtlist[n-1][0] = s_value
				controlinfo/w=setchinput otherunit
				dtlist[n-1][1] = s_value
			endif
			killwindow setchinput
			break
		case "bscincancel":
			killwindow setchinput
			break
		case "bjmyes":
		case "bjmno":
			variable/g ismepsc

			wix = getuserdata("judgewindow","bjmyes","wi")
			nx = getuserdata("judgewindow","bjmyes","n")
			px = str2num(getuserdata("judgewindow","bjmyes","px"))

			wave/t rn
			minipref = rn[9][0]

			if(cmpstr(evt,"bjmyes")==0)
				//		print "xxxxxxxxxxxxxxxxx",px,px/sf
				wn = minipref + wix + "_"+nx
				if(waveexists($"expinfo")==0)
					return ERROR_INVALID_RES_WAVE
				endif
				wave exw = $"expinfo"
				prept = exw[12]
				postpt = exw[13]
				AddmEPSC(wn,px,min(prept,px),postpt)

				ismepsc = 1
			else
				ismepsc = 0
			endif
			if(waveexists($"tmpjwpos"))
				wave tmpjwpos
			else
				make/n=2 tmpjwpos
			endif
			getwindow judgewindow,wsize
			tmpjwpos = {V_left,V_top}
			//	print tmpjwpos[0],tmpjwpos[1]
			killwindow judgewindow						//kill mepsc window, command window, calcium window
			DoWindow/K $tg

			if(strlen(winlist(tgc,";","WIN:7"))!=0)
				DoWindow/K $tgc
			endif
	//		return ismepsc
			break
		case "bjmquit":
			//			controlinfo/w=judgewindow $evt

			make/o/n=(NumberByKey("N_PARAMS", functioninfo(GetRTStackInfo(1))))/t tmpexhis = {evt}
			RecHistory()

			DoWindow/K $tg

			if(strlen(winlist(tgc,";","WIN:7"))!=0)
				DoWindow/K $tgc
			endif
			killwindow judgewindow
			wave/t mnname = $"mnname"
			pt = numpnts(mntime)
			lastwn = mnname[pt-1]
			if(pt>0)
				HighLight(lastwn,0)
			endif
			variable isdebug
			if(waveexists($"expinfo")==0)
				isdebug = FALSE
			else
				wave exw = $"expinfo"
				isdebug = exw[4]
			endif
			dowindow ProgressBar
			if(v_flag)
				KillWindow ProgressBar
			endif
			CleanTemp(!isdebug)

			print "User terminated mEPSC recognition. "+ num2str(numpnts($"mntime"))+" mEPSC added."
			wave/t rn
			//	UniLength(rn[9][1])
			ShowAverage()
			Frequency(0,inf)
			ScaledmEPSC("*",0)
			FitScaledmEPSC("*",0)
			variable/g ismepsc = -9
			break
		case "bjmkill":
			//			controlinfo/w=judgewindow $evt
			make/o/n=(NumberByKey("N_PARAMS", functioninfo(GetRTStackInfo(1))))/t tmpexhis = {evt}
			RecHistory()

			DoWindow/K $tg

			if(strlen(winlist(tgc,";","WIN:7"))!=0)
				DoWindow/K $tgc
			endif
			if(waveexists($"tmpjwpos"))
				wave tmpjwpos
			else
				make/n=2 tmpjwpos
			endif
			getwindow judgewindow,wsize
			tmpjwpos = {V_left,V_top}
			killwindow judgewindow

			//	wavestats/q/m=1 $"mntime"
			//	print v_npnts
			pt = numpnts(mntime)
			if(pt>0)

				wave/t mnname = $"mnname"
				wn = mnname[pt-1]
				//	print wn
				KillmEPSC(wn,-1)
				pt = numpnts(mntime)
				if(pt>0)
					wn = mnname[pt-1]
					HighLight(wn,1)
				endif
			endif

			variable/g ismepsc = -1
			break
		case "bjmmulti":
		case "bjmmtyes":

			killwindows("lockpanel","",0)

			variable/g ismepsc
			wix = getuserdata("judgewindow","bjmyes","wi")
			nx = getuserdata("judgewindow","bjmyes","n")
			px = str2num(getuserdata("judgewindow","bjmyes","px"))
			variable/g sf
			if((waveexists($"expinfo")==0)||(waveexists($"rchmap")==0))
				return ERROR_INVALID_RES_WAVE
			endif
			wave exw = $"expinfo"
			prept = exw[12]
			postpt = exw[13]
			string postwpref
			wave/t rn,rchmap
			i = dimsize(rchmap,0)
			if(i==0)
				return ERROR_NO_TARGET_WAVE
			else
				findvalue/text=(rn[9][2])/txop=2 rchmap
				postwpref = rchmap[mod(v_value,i)][1]
			endif
			wn = postwpref+wix
			if(cmpstr(evt,"bjmmulti")==0)
				//	print px-prept,px+postpt*1.5
				//	print numpnts(tmpm)
				duplicate/o/r=[px-prept,px+postpt*2] $wn,tmpm

				setscale/p x,-prept/sf,1/sf,"s",tmpm
				setaxis/w=$tg bottom, leftx(tmpm),rightx(tmpm)

				ModifyControl bjmmtyes,disable=0,win=judgewindow
				dowindow/f $tg
				ShowInfo/w = $tg
				pauseforuser judgewindow,$tg
			else
				if((strlen(csrinfo(A,tg)))&&(strlen(csrinfo(B,tg))))

					//			print wi,n,px,pcsr(A),pcsr(B)
					variable ps,pe
					ps = min(pcsr(A,tg),pcsr(B,tg))
					pe = max(pcsr(A,tg),pcsr(B,tg))

					wave dw = $"dw"
					variable j = 1e-3*sf
					variable pp

					wave/t rn
					minipref = rn[9][0]

					string wnms,wnme
					wnms = minipref+ wix + "_"+nx

					pp = px + round(0.9*(pe-ps))

					AddmEPSC(wnms,px,prept,pp-px)


					nx = num2str(str2num(nx)+1)

					wavestats/q/m=1/r=[px+pe-prept-j,px+pe-prept] dw
					pe = round(v_maxloc*sf)


					pp = pe +postpt
					//		if(pcsr(C)>-1)
					//			pp = pcsr(C)
					//		endif

					wnme = minipref+ wix + "_"+nx

					AddmEPSC(wnme,pe,0.5*prept,pp-pe)
					HideInfo/w=$tg

					if(waveexists($"tmpjwpos"))
						wave tmpjwpos
					else
						make/n=2 tmpjwpos
					endif
					getwindow judgewindow,wsize
					tmpjwpos = {V_left,V_top}
					killwindow judgewindow
					DoWindow/K $tg
					if(strlen(winlist(tgc,";","WIN:7"))!=0)
						DoWindow/K $tgc
					endif
					ismepsc = 2
		//			return ismepsc
				endif
			endif
			break
		default:
	endswitch
	return ERROR_SUCCESS
End
Override Function CheckSF()
											//checks whether the experiment sampling frequency has been
											//_correctly set or not.
											//details: The global variable sf will be checked. If sf is
											//_consistent with that of a sample trace, nothing will be changed.
											//_Otherwise, the real sampling frequency, calculated as reciprocal
											//_of point-point time interval of acquired data, will be used to
											//_correct this setting.


	variable/g sf

	variable rsf
	if(waveexists($"expinfo")==0)
		return ERROR_INVALID_RES_WAVE
	endif
	wave exw = $"expinfo"

	wave/t rn
	string srcwl = ""
	if(waveexists($"rchmap"))
		wave/t rchmap
		if(dimsize(rchmap,0))
			srcwl = rchmap[0][7]
		else
			srcwl = wavelist(rn[9][1],";","")+wavelist(rn[10][1],";","")+wavelist(rn[11][1],";","")
		endif
	else
		srcwl = wavelist(rn[9][1],";","")+wavelist(rn[10][1],";","")+wavelist(rn[11][1],";","")
	endif
	variable wln = itemsinlist(srcwl)
	string wn
	if(wln!=0)				//do only data wave exists

		wn = stringfromlist(0,srcwl,";")
		if(waveexists($wn))
			rsf = round(1/deltax($wn))
		endif
	else
		return ERROR_NO_TARGET_WAVE
	endif
	if(rsf!=sf)
		print "Set sample frequency according to source data waves. SF = "+num2str(rsf)+"."

		variable prept,postpt
		prept = exw[12]
		postpt = exw[13]

		prept = prept/sf*rsf				//update prept/postpt
		postpt = (postpt+1)/sf*rsf-1

		exw[12] = prept
		exw[13] = postpt
		sf = rsf
	endif
	return ERROR_SUCCESS
End
Override Function GetmEPSC(jmode)
											//finds possible mEPSCs and extracts them from original data.

											//call: UpdateVersion, CheckSF, SetSign, ScanOrg, HighLight, ScaledmEPSC, FitScaledmEPSC, ShowAverage, Frequency, CleanTemp, KillWave, KillWindows
	variable jmode							//judgeMode
											//is a flag used to switch the mEPSC judgement mode.
											//  FALSE    Get mEPSC from the start of all original data.
											//  TRUE    Resume judgement from the saved breaking point.
	String wn								//waveName
											//is the formatted original data wave name
	variable wi								//wave index

	if((waveexists($"expinfo")==0)||(waveexists($"rchmap")==0))
		return ERROR_INVALID_RES_WAVE
	endif

	wave/t rn,rchmap


	variable i = dimsize(rchmap,0)
	if(i==0)
		return ERROR_NO_TARGET_WAVE
	else
		findvalue/text=(rn[9][2])/txop=2 rchmap
	endif
	String wl = rchmap[mod(v_value,i)][7]
	variable/G expsign,sf


	wave exw = $"expinfo"
	variable wln = itemsinlist(wl)
	variable isdebug = exw[4]
	variable postpt = exw[13]
	variable orglength = exw[14]
	variable isgmfin = exw[15]


	if(wln==0)
//		print "No indexed wave."
		return ERROR_NO_TARGET_WAVE
	endif

	CheckSF()

	if((jmode==1)&&((waveexists($"mntime")==0)||(numpnts($"mntime")==0)))		//start instead of resume if no mepsc added
		jmode=0
	//	print "No mEPSC Analyzed Yet."
	endif
	variable orgavg							//original wave average

	string plotmini,plotsmini,plotfsmini,plottimeamp
	wave/t rnw
	plotmini = rnw[0]
	plotsmini = rnw[1]
	plotfsmini = rnw[2]
	plottimeamp = rnw[3]

//	print plotmini,plotsmini,plotfsmini,plottimeamp
	variable ws = 0							//orginial wave start index
	variable ps = 0							//point start index
	variable ns = 0							//mepsc sub index start number
	if(jmode==0)									//initial judge
		killwindows(plotmini,"",1)
		killwindows(plotsmini,"",1)
		killwindows(plotfsmini,"",1)
		killwindows(plottimeamp,"",1)

		killwave("mnname",TRUE,TRUE)
		killwave("mntime",TRUE,TRUE)


		wn = StringFromList(0,wl,";")			//get length(points) of original data wave
		orglength = numpnts($wn)
		exw[14] = orglength

		make/n=0/t mnname						//is the text wave which contains mepsc name information
												//_in the time order. Thus this wave maintains the
												//_map of the result data set to the source wave objects.
												//It is referred and used in most mEPSC analysis
												//_functions. Check it if you meet any runtime error
												//_or unexcepted results.
		make/n=0 mntime						//is the wave where the mEPSC onset time information
												//_is stored. This wave is mainly used to provide the
												//_time-amplitude relationship during mEPSC judgement process.
												//_Also, the time range specified waveform average, frequency
												//_calculation, and properties analyses need this information.
												//NOTE: If you want to remove the added mEPSC waves,
												//_please execute the KillmEPSC function either by clicking
												//_the menu item or using the command line. It will unregister
												//_the information as well as the calculated results if
												//_exist.


		mntime = NaN							//NaN is not calculated when use wavestats

	else										//resume judge
		wave mntime = $"mntime"
		wave/t mnname = $"mnname"
		variable pt = numpnts(mntime)

		wn = mnname[pt-1]						//get break point from lastly identified mepsc




		ws = floor(mntime[pt-1]*sf / orglength)

		ps = mod(mntime[pt-1]*sf,orglength) + postpt
		ns = str2num(wn[strsearch(wn,"_",0,2)+1,inf])+1

	endif

	string winwl,winn									//hide windows
	variable winwln
	winwl = winlist("*",";","win:7")
//	wave/t rnw
	for(i=0;i<4;i+=1)
		winwl = removefromlist(rnw[i],winwl,";",0)
	endfor
	winwln = itemsinlist(winwl)
	for (i=0;i<winwln;i+=1)
		winn = stringfromlist(i,winwl,";")
		Dowindow/HIDE=1 $winn
	endfor

	HideProcedures
	dowindow $"ProgressBar"
	if(v_flag==0)
		NewPanel/FLT=2/N=ProgressBar/W=(800,560,1200,680)/K=2 as "mEPSC judging progress"
		SetActiveSubwindow _endfloat_
		doupdate/w=progressbar
		modifypanel/w=progressbar,framestyle=0,noedit=1

		ValDisplay prgsjms,pos={30,30},size={340,18},limits={0,orglength,0},barmisc={0,0},frame=0,value= _NUM:0,mode=3,win=ProgressBar
		TitleBox pbtxjms,pos={30,15},size={340,18},title="Sweep progress:",frame=0,win=ProgressBar
		ValDisplay prgsjmt,pos={30,85},size={340,18},limits={0,orglength*wln,0},barmisc={0,0},frame=0,value= _NUM:0,mode=3,win=ProgressBar
		TitleBox pbtxjmt,pos={30,70},size={340,18},title="Total progress:",frame=0,win=ProgressBar

		DoUpdate /W=ProgressBar/E=1
	endif
	if(jmode==1)
		killwindows("*average*","",1)
//		Normalize("avg_mepsc","avg_mepsc",3)
		ScaledmEPSC("*",1)
		FitScaledmEPSC("*",1)
		print "Resume mEPSC extraction..."
		wn = StringFromList(ws,wl,";")	//get wave name
//		FilterSpike(wn)
		if (expsign==0)							//check wave sign
			SetSign(wn)
		endif
		ValDisplay prgsjms,value= _NUM:ps,win=ProgressBar
//print ws,ps,ns
//controlinfo/w=progressbar prgsjms
//print v_value
		TitleBox pbtxjms,title="Sweep progress: "+num2str(round(100*ps/orglength))+"%",win=ProgressBar

		ValDisplay prgsjmt,value= _NUM:ws*orglength+ps,win=ProgressBar
		TitleBox pbtxjmt,title="Total progress: "+num2str(round(100*(ws*orglength+ps)/(wln*orglength)))+"%                              ("+num2str(wi+1)+"/"+num2str(wln)+")",win=ProgressBar

		DoUpdate /W=ProgressBar

		ScanOrg(ws,ps,ns)					//scan original wave from break point
		ws+=1

	else
		print "Start mEPSC extraction..."
	endif
	variable we = wln
	if(isdebug)
//		ws = ks
//		we = ke
	endif

	for(wi = ws; wi < we; wi+=1)
		wn = StringFromList(wi,wl,";")	//get wave name
		wavestats/m=1/q $wn
		exw[14] = v_npnts
		if (expsign==0)					//check wave sign
			SetSign(wn)						//set parameters according to pos/neg
		endif
//		FilterSpike(wn)
		ValDisplay prgsjms,value= _NUM:0,win=ProgressBar

		TitleBox pbtxjms,title="Sweep progress: "+num2str(0)+"%",win=ProgressBar
		ValDisplay prgsjmt,value= _NUM:wi*orglength,win=ProgressBar

		TitleBox pbtxjmt,title="Total progress: "+num2str(round(100*(wi/wln)))+"%                              ("+num2str(wi+1)+"/"+num2str(wln)+")",win=ProgressBar

		DoUpdate /W=ProgressBar
		ScanOrg(wi,0,0)					//scan original wave from beginning
	endfor


	if(numpnts(mnname)>0)
		Highlight(mnname[numpnts(mnname)-1],0)		//de-highlight the last displayed mepsc
	endif
	isgmfin = 1
	exw[15] = isgmfin
	dowindow ProgressBar
	if(v_flag)
		KillWindow ProgressBar
	endif
	CleanTemp(!isdebug)											//clean up temporal waves
	wave/t rn
//	UniLength(rn[9][1])
	ShowAverage()

	Frequency(0,inf)
	ScaledmEPSC("*",0)
	FitScaledmEPSC("*",0)
	UpdateVersion()
	return ERROR_SUCCESS
End
Override Function SetSign(wn)
											//judges the sign of base line and adjusts the time window for mEPSC extaction.
											//call: KillWave
	string wn								//waveName
											//is the name of wave which will be used to calculate the sign of the
											//_baseline.
	if(waveexists($wn)==0)
		return ERROR_INVALID_WAVE
	endif
	variable/g expsign
	if(expsign==0)
		integrate $wn /d=tmpsign
		expsign = sign(tmpsign[inf])
		killwave("tmpsign",TRUE,TRUE)
	endif
	return ERROR_SUCCESS
//	if(waveexists($"expinfo")==0)
//		return //-1
//	endif
//	wave exw = $"expinfo"
//	variable prept,postpt
//	if(prept+postpt==0)
//		variable/g sf
//		prept = 2e-3 * sf
//		postpt = 5e-3 * sf -1
//	endif
End
Override Function ScanOrg(owi,ps,ns)
											//scans original data wave to get possible mEPSCs.
											//details: During mEPSC discrimination process, source waves will
											//_be firstly examined by program-based filter. The signals which
											//_passed the filter will be present to user for further recognition.
											//A control panel will be shown for user to decide if the signal is
											//_a real mEPSC or just an artifact. Scaled- and fitted- signals and
											//_time-amplitude information will be available for judgement.
											//call: ButtonListener, KeyListener, AutoFilter, AddmEPSC, ShowWave, ModifyWave, KillWindows
	variable owi							//orgWaveIndex
											//is the index of source wave from which the mEPSC will be
											//_extracted.
	variable ps								//startPoint
											//is the point of original data wave where the scanning window
											//_starts.
	variable ns								//destSubIndex
											//is the start sub-index of target mEPSC.

	string postwpref,prewpref
	if((waveexists($"expinfo")==0)||(waveexists($"rchmap")==0))
		return ERROR_INVALID_RES_WAVE
	endif

	wave/t rn,rchmap
	variable i = dimsize(rchmap,0)
	if(i==0)
		return ERROR_NO_TARGET_WAVE
	else
		findvalue/text=(rn[9][2])/txop=2 rchmap
		postwpref = rchmap[mod(v_value,i)][1]
		findvalue/text=(rn[11][2])/txop=2 rchmap
		if(v_value!=-1)
			prewpref = rchmap[mod(v_value,i)][1]
		else
			prewpref = ""
		endif
	endif


	String wn = postwpref+num2str(owi)

	variable/G SF,datatype

	variable prept,postpt,ampth,orglength

	wave exw = $"expinfo"
	variable wln = exw[1]
	variable isdebug = exw[4]
	ampth = exw[10] * 1e-12
	prept = exw[12]
	postpt = exw[13]
	orglength = exw[14]

	variable n = ns						//mipw data counter

	duplicate/o $wn, ow					//make original wave copy
	ow = abs(ow)							//make it positive

	duplicate/o ow,bw,sw					//make full-length wave for correction


	smooth min(32767,sf),bw
	sw-=bw									//slope correction, base shift correction
											//local based

	ow-=bw


	variable smd = 1e-3*sf					//smooth length

	Smooth smd,sw
	smooth smd,sw
//	duplicate/o sw,bw
//	print "sw"

//	insertpoints 0,bd,bw
//	deletepoints orglength,bd,bw
//	bw[0,bd-1] = bw[bd]
//	tw -= bw

	differentiate sw/d=dw					//differential wave

	dw/=sf
//	duplicate/o bw,$"bw"+num2str(owi)
//	duplicate/o sw,$"sw"+num2str(owi)
//	duplicate/o dw,$"dw"+num2str(owi)

	variable pf								//pass filter

	variable px								//differential peak x-point
	variable pp								//point for next

	variable autojudge = exw[16]			//use auto judgement (1==consider all events passed filter as signals)
//	autojudge = 1
	string 	tg = "tmpmg",tgc = "tmpcag"
	for (i=ps;i<orglength;i+=1)
		pf = 1
		if(mod(i,round(0.01*sf))==0)
//		print i
		ValDisplay prgsjms,value= _NUM:i,win=ProgressBar

		TitleBox pbtxjms,title="Sweep progress: "+num2str(round(100*i/orglength))+"%",win=ProgressBar
		ValDisplay prgsjmt,value= _NUM:owi*orglength+i,win=ProgressBar

		TitleBox pbtxjmt,title="Total progress: "+num2str(round(100*(owi*orglength+i)/(wln*orglength)))+"%                              ("+num2str(owi+1)+"/"+num2str(wln)+")",win=ProgressBar

		DoUpdate /W=ProgressBar
		endif
		if ((sw[i]>ampth))//||(dw[i]>ampth))
			px = i
			pf = AutoFilter(px,pp)

			if(pf==ERROR_MINI_SUCCESS)			//pass auto check, may be mEPSC
		//		print px/sf
				if(autojudge)
					string minipref = rn[9][0]
					string wnm = minipref + num2str(owi) + "_"+num2str(n)
					if(waveexists($"expinfo")==0)
						return ERROR_INVALID_RES_WAVE
					endif
				//	wave exw = $"expinfo"

					AddmEPSC(wnm,px,min(prept,px),postpt)
					n+=1
					i=px+postpt
		//			ismepsc = 1
				else

					duplicate/o/r=[px-prept,px+postpt] $wn,tmpm

					setscale/p x,-min(prept,px)/sf,1/sf,"s",tmpm

					ShowWave("tmpm",1)

					if(isdebug)
						//		print "xxx"
						duplicate/o/r=[px-prept,px+postpt] sw,tmp

						tmp = -tmp+tmpm[0]
						setscale/p x,-min(prept,px)/sf,1/sf,"s",tmp
						//										display tmp
						//					abort
						appendtograph/w=$tg tmp
						ModifyWave(tg,"tmp",1,1.5)
						//					doupdate
					endif
					Dowindow/t $tg,wn+" px="+num2str(px)+" t="+num2str(px/sf)+"s"
					if(datatype&4)

						string prewn = prewpref + num2str(owi)
						duplicate/o/r=[px-prept,px+postpt] $prewn,tmpca
						setscale/p x,-min(prept,px)/sf,1/sf,"s",tmpca
						ShowWave("tmpca",2)

						dowindow/t $tgc, "Presynaptic Segment"
					endif
					DoWindow/F $tg

					if(waveexists($"tmpjwpos"))
						wave tmpjwpos
					else
						getwindow $tg,wsize
						make/n=2 tmpjwpos = {V_left,V_top + V_bottom+50}
						if((datatype&4)==0)
							tmpjwpos = {V_left+V_right+50,V_top+50}
						endif
					//	AutoPositionWindow/E/M=1/R=$tg judgewindow// Put panel near the graph
					endif
					NewPanel/FLT=2/K=2/N=judgewindow/W=(tmpjwpos[0],tmpjwpos[1],tmpjwpos[0]+150,tmpjwpos[1]+120) as "Pause for Judge"
					movewindow/w=judgewindow tmpjwpos[0],tmpjwpos[1],tmpjwpos[0]+150,tmpjwpos[1]+120
					SetActiveSubwindow _endfloat_
					doupdate/w=judgewindow
					modifypanel/w=judgewindow,framestyle=0,noedit=1
					killwindows("lockpanel","",0)
					display/W=(21000,21000,21000,21000)/K=1/n=lockpanel as ""
					SetActiveSubwindow lockpanel

					setwindow lockpanel,hook(listener)=KeyListener
				//	DoWindow/C/w=tmppanel judgewindow // Set to an unlikely name

					Button bjmyes,pos={20,25},size={40,20},title="Yes",win=judgewindow
					Button bjmyes,proc=ButtonListener,win=judgewindow
					Button bjmyes userdata(wi)=num2str(owi),win=judgewindow
					Button bjmyes userdata(n)=num2str(n),win=judgewindow
					Button bjmyes userdata(px)=num2str(px),win=judgewindow



					Button bjmno,pos={130,25},size={40,20},title="No",win=judgewindow
					Button bjmno,proc=ButtonListener,win=judgewindow
					Button bjmmulti,pos = {20,65},size={100,20},title="Multiple Peak",win=judgewindow
					Button bjmmulti,proc=ButtonListener,win=judgewindow
					Button bjmmtyes, pos = {144,65},size = {25,20},title = "OK",disable=3,win=judgewindow
					Button bjmmtyes,proc=ButtonListener,win=judgewindow
					Button bjmkill,pos={20,105},size={80,20},title="Remove Last",win=judgewindow
					Button bjmkill,proc=ButtonListener,win=judgewindow
					Button bjmquit,pos={130,105},size={40,20},title="Stop",win=judgewindow
					Button bjmquit,proc=ButtonListener,win=judgewindow

					PauseForUser judgewindow,lockpanel

					variable/g ismepsc

					switch(ismepsc)				//skip points
						case 1:						//yes
							n+=1
							i=px+postpt
							break
						case 0:						//no
							i = pp
							break
						case 2:						//multiple peak
							n+=2
							i=px+postpt
							break
						case -1:					//remove last
							wave mntime = $"mntime"
							wave/t mnname = $"mnname"
							variable pt = numpnts(mntime)

							variable lastwi = floor(mntime[pt-1]*sf / orglength)

							//	print lastwi,wi

							if(lastwi==owi)
								i = mod(mntime[pt-1]*sf,orglength) +postpt
								//		print i
							else
								i=-1
							endif
							if(n>0)
								n-=1
							endif
							break
						case -9:
							abort
							break
						default	:
							//			print "User end"
					endswitch
				endif
			else								//filtered, not qualified
				i = pp
		//		mipw[n] = NaN
			endif
		endif
	endfor
	return ERROR_SUCCESS
End
Override Function AutoFilter(px,pp)
											//automatically filters mEPSCs during judgement.
											//details: The following rejection criteria are used to wipe out the
											//_artifacts and noises.
											//  1. Amplitude is larger than the maximum threshold.
											//  2. The signal is too narrow (too short time between onset and peak).
											//  3. The dblexp fit function to signal meets error.
											//  4. The signal fits better with line than dblexp.
											//When a signal is recognized as noise, the operation will positively
											//_shift the values of px and pp and return -1. Otherwise, px will be set to
											//_differential peak (onset) time point.
	variable &px							//onSetPoint
											//is used to specify the mEPSC onset time offset (in point) in the
											//_original wave. It will be set to -1 by the function if the signal
											//_fails to pass the filter.
	variable &pp							//tailPoint
											//is the end point of the checked signal. It is also used as the
											//_start point for the next judgement calculations.
	variable/g SF
	if(waveexists($"expinfo")==0)
		return ERROR_INVALID_RES_WAVE
	endif
	wave exw = $"expinfo"

	variable ampth,ampmax,prept,postpt,orglength
	variable isdebug = exw[4]
	variable allowmipsc = exw[17]
	ampth = exw[10]*1e-12
	ampmax = exw[11]*1e-12
	prept = exw[12]
	postpt = exw[13]
	orglength = exw[14]

	wave ow = $"ow"
	wave dw = $"dw"
	wave sw = $"sw"

	variable dy,pt
	variable amp,bamp,namp,tamp,loc		//base/negtive(opposite)/tail amplitude
	variable i,j,k,rw

	i = px
	j = 1e-3*sf
	if(allowmipsc)
		j = 10e-3*sf
	endif
	k = 2e-4*sf								//
//abort

	if(sw[i]>ampth)														//Amplitude Judge
		for(i=px;i<orglength-1;i+=1)
			if(sw[i+1]<sw[i])
				break
			endif
		endfor
		pp = i												//Amplitude peak point
		if(pp-px<k)
			for(;i<orglength-1;i+=1)
				if(sw[i]<ampth)
					break
				endif
			endfor
			pp = i
			if(isdebug)
				print px/sf,pp/sf,"signal is too narrow"
			endif
			return ERROR_MINI_TOO_NARROW

		endif
		wavestats/q/m=1/r=[pp-j,pp] dw
		px = round(v_maxloc*sf)
//		if((pp-px)>0.6*j)
//			pp += j
//			if(isdebug)
//				print px/sf,pp/sf,"onset too slow"
//			endif
//			return //-1
//
//		endif
	endif

	if((sw[pp]>=ampmax)||(ow[pp]>=ampmax))						//amplitude too large
		for(i=pp;i<orglength-1;i+=1)
			if(sw[i]<0.5*ampmax)
				break
			endif
		endfor

		pp = 2 * i - pp
		if(isdebug)
			print px/sf,pp/sf,"signal is too large"
		endif

		return ERROR_MINI_TOO_BIG
	endif


//	if(px<prept)
//		pp = prept
//		if(isdebug)
//			print px/sf,pp/sf, "baseline length too short"
//		endif
//		return //-1
//	endif


//	if(px+postpt>=orglength-1)
//		if(isdebug)
//			print px/sf,pp/sf,"signal is too close to end"
//		endif
//		pp = orglength-1
//		return //-1
//	endif
	if(allowmipsc)
		return ERROR_MINI_AUTO_JUDGE
	endif
	variable baseamp = mean(ow,(px-prept)/sf,(px-0.1*prept)/sf)
	variable tailamp = mean(ow,(px+0.8*postpt)/sf,(px+postpt)/sf)
	if(abs(tailamp-baseamp)>0.5*abs(ow[pp]-baseamp))
		return ERROR_MINI_NOT_BACK
	endif
	variable v_fiterror
	variable chidblexp,chiline,chiexp
	CurveFit/NTHR=0/Q/N/W=2 dblexp, sw[pp,pp+postpt]
	wave w_coef
	for(i=1;i<5;i+=1)
		if(w_coef[i]<0)
			if(isdebug)
				print px/sf,pp/sf,"signal has negative decay parameter(s)"
			endif
			pp += pp-px
			return ERROR_MINI_DXPFIT_ERR
		endif
	endfor
	if(v_fiterror!=0)
		if(isdebug)
			print px/sf,pp/sf,"signal has dblexp fiterror"
		endif
//		pp += pp-px
//		return ERROR_MINI_DXPFIT_ERR
	else
		chidblexp = v_chisq
		CurveFit/NTHR=0/Q/N/W=2 line, sw[pp,pp+postpt]
		if(v_fiterror!=0)
			//line fit error
		else
			chiline=v_chisq
			if(chidblexp>chiline)
				CurveFit/NTHR=0/Q/N/W=2 exp, sw[pp,pp+postpt]
				if(v_fiterror!=0)
					//exp fit error
				else
					chiexp = v_chisq
					if(chiexp>chiline)
						if(isdebug)
							print px/sf,pp/sf,"signal is more likely to line than exp/dblexp"
						endif

						pp += pp-px
						return ERROR_MINI_LIKE_LINE
					endif
				endif
			endif
		endif
	endif

if(1==3)
	wavestats/m=1/q/r=[i-0.5e-3*SF,i+0.5e-3*SF] dw
	dy=dw[i]
	if(v_min<-0.75*dy)
	//	mipw[n] = -1
	//	return //-1
	endif

	wavestats/m=1/q/r=[i-prept,i-0.2*prept] ow		//Get local base
	bamp = v_avg

	wavestats/m=1/q/r=[i-1e-3*SF,i+2e-3*SF] ow
	amp = v_max - bamp
	if (amp<ampth)					//Filter too-small wave
	endif
	amp = v_max-v_min					//peak-to-peak amplitude
	if (amp>ampmax)					//Filter too-large wave
	endif



	wavestats/m=1/q/r=[i-prept,i+postpt] ow		//Get local base
	namp = bamp-v_min
	amp = v_max-bamp

	if (namp>0.4*amp)					//Filter wave with opposite noise
	//	mipw[n] = -1
	//	return //-1
	endif

	wavestats/m=1/q/r=[i+0.75*postpt,i+postpt] ow	//Get tail base
	tamp = v_avg

	if(abs(tamp-bamp)>0.2*amp)		//Filter wave not or over return to base line
	//	mipw[n] = -1

	//	return //-1
	endif


	duplicate/o/r=[i-prept,i+postpt] ow,tmp
	wavestats/m=1/q tmp
	loc = v_maxloc * SF -i

	if (loc>(prept+0.5*postpt))		//Filter wave with other wave(s) in reading frame
	//	mipw[n] = -1
	endif

/////////////////old code,not use///
//	if(1==2)											//Differential judge
		wavestats/q/m=1/r=[i,i+j] dw

	//	v_maxloc*sf	-i<j									//Differential peak point
		pp = px
		if((abs(v_maxloc-v_minloc)<1e-4)||(v_min<-0.5*v_max)||(px-i>5e-4))
		//	print "d-d",i/sf
			k = NaN
		endif
		if(k)
			wavestats/q/m=1/r=[i,i+j] ow
			pp = v_maxloc*sf									//Amplitude peak point
			if((abs(v_maxloc-v_minloc)<1e-4)||(v_max-ow[pp-prept]<ampth)||(pp<px)||(v_min<-0.5*v_max)||(pp-px)>5e-4*sf)
			//	print "d-a",i/sf
				k = NaN
			endif

		endif
	endif
	if(1==3)
		wavestats/q/m=1/r=[i,i+j] ow

		pp = v_maxloc*sf										//Amplitude peak point

		if((abs(v_maxloc-v_minloc)<1e-4)||(v_max-ow[pp-prept]<ampth)||(v_min<-0.5*v_max)||(pp-i)>5e-4*sf)
		//	print "a-a",i/sf
			k = NaN
		endif
		if(k)
			wavestats/q/m=1/r=[i,i+j] dw
			px = v_maxloc*sf									//Differential peak point
			if((abs(v_maxloc-v_minloc)<1e-4)||(pp<px)||(pp-px)>5e-4*sf)
			//	print "a-d",i/sf
				k = NaN
			endif
		endif
	endif


//	print tamp,namp,amp
	return ERROR_MINI_SUCCESS

End
Override Function AddmEPSC(wn,px,preps,postps)
											//are executed when user confirms mEPSCs during judgement.
											//_New mEPSC waves are named, registered and displayed.
											//details: AddmEPSC creates mEPSC and corresponding presynaptic waves (if pre- source waves exist)
											//_with formatted names. It registers mEPSC timing and name information and
											//_appends the waves to plot graphs.
											//Presynaptic waves will be created only when the channel configuration is set to
											//_multiple channel recording with one or more presynaptic signal type(s) selected.

											//call: HighLight, Base, PlotmEPSC, ScaledmEPSC, FitScaledmEPSC, TimemEPSC, RecHistory
	string wn								//waveName
											//is the mEPSC wave name.
	variable px								//onsetPt
											//is the offset of the mEPSC onset point in the original data wave.
	variable preps							//prePt
											//is the time window width (in point) before mEPSC onset time point.
	variable postps						//postPt
											//is the time window width (in point) after mEPSC onset time point.
	make/o/n=(NumberByKey("N_PARAMS", functioninfo(GetRTStackInfo(1))))/t tmpexhis = {wn,num2str(px),num2str(preps),num2str(postps)}
	RecHistory()
	variable/g sf,datatype
	string owi								//orgWaveIndex
											//is the index of original wave where comes the identified mEPSC.
	string wno,wnc
	string prewpref,postwpref,minipref,capref
	if((waveexists($"expinfo")==0)||(waveexists($"rchmap")==0))
		return ERROR_INVALID_RES_WAVE
	endif

	wave/t rn,rchmap
	minipref = rn[9][0]
	capref = rn[11][0]
	variable i = dimsize(rchmap,0)
	if(i==0)
		return ERROR_NO_TARGET_WAVE
	else
		findvalue/text=(rn[9][2])/txop=2 rchmap
		postwpref = rchmap[mod(v_value,i)][1]
		findvalue/text=(rn[11][2])/txop=2 rchmap
		if(v_value!=-1)
			prewpref = rchmap[mod(v_value,i)][1]
		else
			prewpref = ""
		endif
	endif
	variable vp
	vp = strsearch(wn,"_",0)
	owi = wn[strlen(minipref),vp-1]
	wno = postwpref + owi

	duplicate/o/r=[px-preps,px+postps] $wno,$wn
	setscale/p x,-preps/sf,1/sf,"s",$wn
	Base(wn,preps)				//make mepsc base line to zero

	if(datatype&4)
		wno = prewpref + owi
		wnc = capref + wn[strlen(minipref),inf]
		duplicate/o/r=[px-preps,px+postps] $wno,$wnc
		setscale/p x,-preps/sf,1/sf,"s",$wnc
		Base(wnc,preps)
	endif


	PlotmEPSC(wn)			//add mepsc to plot window
	ScaledmEPSC(wn,1)		//add scaled mepsc to plot window
	FitScaledmEPSC(wn,1)	//add dblexp function fitted scaled mepsc to plot window

	wave mntime = $"mntime"
	wave/t mnname = $"mnname"
	variable pt = numpnts(mntime)			//get previous mepsc information
	string lastwn = mnname[pt-1]
	if(pt>0)								//de-high-light the previously added mepsc
		HighLight(lastwn,0)
	endif


	variable/g sf

	wave exw = $"expinfo"
	variable orglength = exw[14]
	variable flpx = (orglength * str2num(owi) + px)/sf

	insertpoints inf,1,mntime,mnname		//register mepsc time & name information
	mntime[pt]=flpx
//	print wn
	mnname[pt] = wn
	TimemEPSC()								//update amplitude-timing graph
	HighLight(wn,1)						//high-light the lastly added mepsc
	return ERROR_SUCCESS
End
Override Function HighLight(wn,hlmode)
											//highlights and de-highlights the specific mEPSC wave.
											//call: ModifyWave
	string wn								//waveName
	variable hlmode						//highLightMode
											//is a switch to alter the highlight mode.
											//  FALSE    Dehighlight (Set color to red and size to 1).
											//  TRUE    Highlight (Set color to black and size to 1.5).

	if(waveexists($wn)==0)
		return ERROR_INVALID_WAVE
	endif
	String plotmini, plotsmini,plotfsmini
	wave/t rnw
	plotmini = rnw[0]
	plotsmini = rnw[1]
	plotfsmini = rnw[2]

	String lwn = wn
	String scllwn = "mnscale"//"scaled_"+lwn
	String sclfitlwn = "mnfit"//+scllwn
	wave/t mnname
	FindValue /TEXT=wn/TXOP=2 mnname
//	print v_value,wn
//	if(v_value<0)
//		v_value = numpnts(mnname)
//	endif
	if(v_value!=0)
		scllwn +="#"+num2str(v_value)
		sclfitlwn +="#"+num2str(v_value)
	endif
	ModifyWave(plotfsmini,sclfitlwn,-hlmode,1+0.5*hlmode)
	ModifyWave(plotsmini,scllwn,-hlmode,1+0.5*hlmode)
	ModifyWave(plotmini,lwn,-hlmode,1+0.5*hlmode)
	return ERROR_SUCCESS
End
Override Function Base(wn,bp)
											//returns baseline mean value of the wave named wn and adjusts it to zero.
											//details: When correcting the baseline level, the mean value of 0.9 * bp points
											//_will be calculated as original base and returned. The wave wn will be subtracted
											//_by this value to make the baseline as zero.
											//call: SetError
	String wn								//waveName
											//is the name of target wave to be adjusted.
	variable bp								//baseLength
											//is the length (in points) of baseline.
	variable ba = NaN

	if(waveexists($wn))
		wave w = $wn
		ba = mean(w,pnt2x(w,0),pnt2x(w,0.9*bp))
		w -= ba
	else
		SetError(ERROR_INVALID_WAVE)
	endif
	return ba
End
Override Function PlotmEPSC(wn)
											//adds the specified mEPSC wave to the plot window.
											//call: ShowWave
	String wn								//waveName
											//is the name of target mEPSC wave.
	String plotmini
	wave/t rnw
	plotmini = rnw[0]

	String windowlist = winlist(plotmini,";","Win:7")
	variable windowln = ItemsInList(windowlist)
	//				print"windowln=",windowln

	if (windowln ==0)
		ShowWave(plotmini,5)

		AppendToGraph/w=$plotmini $wn
	//	ModifyGraph /w=$plotwindow grid(bottom)=2
	//	ModifyGraph /w=$plotwindow grid(left)=2
	else


		//				print "add to miniplot"
		AppendToGraph/w=$plotmini $wn


	endif
	return ERROR_SUCCESS
End
Override Function ScaledmEPSC(wn,smode)
											//scales the specified mEPSC wave and adds it to a plot window.
											//details: It is used to show the mEPSC kinetics during the
											//_manual judgement. The identified mEPSC wn will be scaled to 1
											//_and displayed.
											//If wn is an empty string or is "*", the function will
											//_try to scale all the mEPSC waves according to smode.
											//call: ConcatWave, UniLength, ShowWave
	String wn								//waveName
											//is the name of wave which will be scaled.
											//_The name of output wave will be formatted as "scaled_"+wn.
	variable smode							//scaleMode
											//is used to decide which norm the waves should be scaled to. The
											//_value could be:
											//  FALSE    Use averaged amplitude as norm.
											//  TRUE    Use number 1 as norm.

	string scwn = "mnscale"

	string plotsmini
	wave/t rnw
	plotsmini = rnw[1]

	variable/g expsign
	wave/t mnname = $"mnname"
	wave mntime = $"mntime"
	if(waveexists(mntime)==0)
		return ERROR_NO_TARGET_WAVE
	endif


	variable miniwln = numpnts(mntime)
	variable maxy,miny

	wave exw = $"expinfo"
	variable prept = exw[12]
	variable/g sf
	if(waveexists($wn))

		if(waveexists($scwn)==0)
			make/n=(0,0) $scwn
			setscale/p x,-prept/sf,1/sf,"s",$scwn
			setscale/p d,0,0,"A",$scwn
		endif
		duplicate/o $wn,tmpscale
		UniLength("tmpscale",FALSE)
		wavestats/m=1/q tmpscale
		if(expsign==1)
			tmpscale *= 1/v_max
		else
			tmpscale *= -1/v_min
		endif

		wave w = $scwn
		//		concatwave("tmpscale",0,0,FALSE,scwn)

		concatwave("tmpscale",0,0,FALSE,scwn)
		variable coln = dimsize(w,1)-1
//		redimension/n=(numpnts(tmpscale),coln+1) w
//		w[][coln] = tmpscale
		String windowlist = winlist(plotsmini,";","Win:7")
		variable windowln = ItemsInList(windowlist)
	//	print wn,dimsize(w,1)-1
		if (windowln==0)
			ShowWave(plotsmini,3)
			AppendToGraph/w=$plotsmini w[][coln]
	//		ModifyGraph /w=$scaledwindow grid(bottom)=2
	//		ModifyGraph /w=$scaledwindow grid(left)=2
			wavestats/m=1/q w

			maxy = max(v_max,0)
			miny = min(v_min,0)
			SetAxis /w=$plotsmini left, 1.05*miny,1.05*maxy
		else

			AppendToGraph/w=$plotsmini w[][coln]
		endif



	else
		if(cmpstr(wn,"*")==0)
			if(numpnts($"mntime")==0)
				return ERROR_NO_TARGET_WAVE
			endif
			variable wi

			variable amp,avg
			if(smode==1)
				avg = 1
			else
				avg = mean($"amp")
			endif
			wave w = $scwn
			wavestats/m=1/q w
			if(expsign==1)
				w *= avg/v_max
			else
				w *= -avg/v_min
			endif
			wavestats/m=1/q w
			maxy = max(v_max,0)
			miny = min(v_min,0)
			SetAxis /w=$plotsmini left, 1.05*miny,1.05*maxy

		endif
	endif
	return ERROR_SUCCESS
End
Override Function FitScaledmEPSC(wn,smode)
											//fits the the scaled mEPSC wave with double exponential function
											//_and adds it to the plot window.
											//details: It is used to show the mEPSC kinetics during the
											//_manual judgement. The identified mEPSC wn will be scaled to 1
											//_and fitted by double-exponential function and displayed.
											//If wn is an empty string or is "*", the function will
											//_try to scale all the fitted waves according to smode.
											//call: ConcatWave, ShowWave
	String wn								//waveName
											//is the name of wave which will be scaled and fitted.
											//_The name of output wave will be formatted as "fit_scaled_"+wn.
	variable smode							//scaleMode
											//is used to decide which norm the waves should be scaled to. The
											//_value could be:
											//  FALSE    Use averaged amplitude as norm.
											//  TRUE    Use number 1 as norm.


	variable/g expsign,SF
	String plotfsmini
	wave/t rnw

	plotfsmini = rnw[2]

	string scwn = "tmpscale"//sclpref+wn
	string fscwn = "mnfit"
	wave/t mnname = $"mnname"
	wave mntime = $"mntime"
	wave exw = $"expinfo"
	variable isdebug = exw[4]

	variable miniwln = numpnts(mntime)

	variable maxy,miny

	wave exw = $"expinfo"
	variable prept = exw[12]
	variable/g sf


	if(waveexists($scwn))
		wave tmpscale = $scwn
		wavestats/q tmpscale
		variable py
		if(expsign==1)
			py = v_maxloc
		else
			py = v_minloc
		endif

		if(waveexists($fscwn)==0)
			make/n=(0,0) $fscwn
			setscale/p x,-prept/sf,1/sf,"s",$fscwn
			setscale/p d,0,0,"A",$fscwn
		endif
		duplicate/o $scwn,tmpfitscale
		variable V_FitError

		CurveFit/NTHR=0/Q/N/W=2 dblexp, tmpfitscale[x2pnt(tmpfitscale,py),inf]
		if((v_fiterror!=0)&&(isdebug))
			print scwn+" fit error. ErrorCode:",v_fiterror
		endif
		tmpfitscale = NaN
		if(v_fiterror==0)
			tmpfitscale[x2pnt(tmpfitscale,py),inf] = K0+K1*exp(-K2*x)+K3*exp(-K4*x)
		endif
	//	UniLength("tmpfitscale",FALSE)


		wave w = $fscwn
		//		concatwave("tmpscale",0,0,FALSE,scwn)

		concatwave("tmpfitscale",0,0,FALSE,fscwn)
		variable coln = dimsize(w,1)-1
//		redimension/n=(numpnts(tmpscale),coln+1) w
//		w[][coln] = tmpscale
		String windowlist = winlist(plotfsmini,";","Win:7")
		variable windowln = ItemsInList(windowlist)
	//	print wn,dimsize(w,1)-1
		if (windowln==0)
			ShowWave(plotfsmini,4)
			AppendToGraph/w=$plotfsmini w[][coln]
	//		ModifyGraph /w=$scaledwindow grid(bottom)=2
	//		ModifyGraph /w=$scaledwindow grid(left)=2
			wavestats/m=1/q w

			maxy = max(v_max,0)
			miny = min(v_min,0)
			SetAxis /w=$plotfsmini left, 1.05*miny,1.05*maxy
		else

			AppendToGraph/w=$plotfsmini w[][coln]
		endif

	else
		if(cmpstr(wn,"*")==0)
			if(numpnts($"mntime")==0)
				return ERROR_NO_TARGET_WAVE
			endif
			variable wi

			variable amp,avg
			if(smode==1)
				avg = 1
			else
				avg = mean($"amp")
			endif
			wave w = $fscwn
			wavestats/m=1/q w
			if(expsign==1)
				w *= avg/v_max
			else
				w *= -avg/v_min
			endif
			wavestats/m=1/q w
			maxy = max(v_max,0)
			miny = min(v_min,0)
			SetAxis /w=$plotfsmini left, 1.05*miny,1.05*maxy

		endif
	endif
	return ERROR_SUCCESS
End
Override Function TimemEPSC()
											//shows and updates the graph of mEPSC amplitude-timing
											//_information.
											//call: ShowWave
	variable/g expsign

	wave mntime =$"mntime"
	wave/t mnname =$"mnname"
	wave/t rn = $"rn"
	if(waveexists($rn[1][0])==0)
		make/o/n=0 $rn[1][0]

	endif
	wave amp = $rn[1][0]
	variable pt = numpnts(mntime)

	variable px = mntime[pt-1]
	string wn = mnname[pt-1]
	variable/g sf
	insertpoints inf,1,amp
	if(expsign==1)
		amp[pt-1] = wavemax($wn)
	else
		amp[pt-1] = abs(wavemin($wn))
	endif
	string plottimeamp
	wave/t rnw
	plottimeamp = rnw[3]

	if(strlen(winlist(plottimeamp,";","Win:7"))==0)
		ShowWave(plottimeamp,6)


		AppendToGraph amp vs mntime
		ModifyGraph mode=1
		setscale/i y,0,0,"A",amp
		setscale/i y,0,0,"s",mntime
		SetAxis left 0,*
		SetAxis bottom 0,*
	endif
	return ERROR_SUCCESS
End
Override Function GeteEPSC(twi,fmode)
											//gets eEPSC waves from original data.
											//call: UpdateVersion, CheckSF, SetSign, RemovePseudo, KillWave
	variable twi							//trainIndex
											//is used to specify the original train from which the eEPSC would be
											//_obtained by segmentation. The value could be:
											//  -2    User select.
											//  -1    All trains.
											//  i    The train indexed i.
	variable fmode							//freqMode
											//is a flag to decide which calculation method will be used to
											//_extract the eEPSC waves.
											//  FALSE    Fixed frequency protocol for a single train.
											//  TRUE    Various frequency protocol for a single train.

	String wn	//formatted original wave name
	if((waveexists($"expinfo")==0)||(waveexists($"rchmap")==0))
		return ERROR_INVALID_RES_WAVE
	endif

	wave/t rn,rchmap


	variable i = dimsize(rchmap,0)
	if(i==0)
		return ERROR_NO_TARGET_WAVE
	else
		findvalue/text=(rn[9][2])/txop=2 rchmap
	endif
	String srcwl = rchmap[mod(v_value,i)][7]
	string orgwl = rchmap[mod(v_value,i)][6]
	string epscpref,srcwpref,srcwlpref

	epscpref = rn[10][0]
	srcwpref = rchmap[v_value-2*i][2]
	srcwlpref = srcwpref + "*"


	variable/G expsign,sf

	wave exw = $"expinfo"
	variable wln = exw[1]

	if(wln==0)
	//	print "No Indexed Wave."
		return ERROR_NO_TARGET_WAVE
	endif


	CheckSF()

	HideProcedures
	variable singlepos				//single original wave sign

	variable orgavg				//original wave average

	if((twi<-1)||(twi>=wln))
		print "Wrong train index."
		return ERROR_INDEX_OUT_OF_RANGE
	endif
	if(twi==ALL)
		doalert/t="" 1,"Do source waves contain various frequency trains?"
		if(v_flag==1)
			fmode = 1
			make/o/n=(wln*24,5) stimfiber
			make/o/n=(24,2) stiminter
			stiminter[][1] = 20
			stiminter[0][1] = 49
	//		stiminter[0][1] = 50			//5ms ISI
			stiminter[18][1] = 2			//300ms ISI
			stiminter[][0] = {5,6,8,10,12.5,15,20,25,30,40,50,60,80,100,125,150,200,250,300,400,500,600,800,1000}
			for(twi=0;twi<wln;twi+=1)
				string wni
				wn = srcwpref+num2str(twi)
				variable wii,interlen,inters
				inters = 0
				interlen = 0
				inters = 0.25*sf
				for(wii=0;wii<24;wii+=1)
					wni = wn+"_"+num2str(wii)
					interlen = stiminter[wii][0]* stiminter[wii][1]*sf/1000

					duplicate/o/r=[inters,inters+interlen-1] $wn,$wni
					setscale/p x,0,1/sf,"s",$wni
					setscale/p y,0,0,"A",$wni
					stimfiber[twi*24+wii][] = 0
					stimfiber[twi*24+wii][1] = 1000/stiminter[wii][0]
					stimfiber[twi*24+wii][2] = stiminter[wii][1]
					inters += interlen
				endfor
			//	killwave/z $wn
				rename $wn, $"b"+wn
			endfor
		//	postwl = wavelist(postwlpref,";","")
			wln = itemsinlist(srcwl)
		else
			fmode = 0
			make/o/n=(wln,5) stimfiber				//is a wave that holds fiber stimulation information.
														//It is used to segment the original waves into eEPSCs
														//_according to the stimulation protocols. Meanwhile,
														//_the pseudo trace elimination process sets and puts
														//_the time information of artifacts into this wave.
														//stimfiber is a (n,5) two-dimensional wave (matrix),
														//_where n stands for the number of the eEPSC trains.
														//_The columns are as follows:
														//  column 0:    Time before the first stimulus of the train.
														//  column 1:    Stimulation frequency of the train.
														//  column 2:    Stimuli number in the train.
														//  column 3:    Pseudo trace start point.
														//  column 4:    Pseudo trace end point.
		endif


	//	waitt,stimf,stimn,pseudot,pseudob
		for(twi=0;twi<wln;twi+=1)
			Geteepsc(twi,fmode)
		endfor
		print num2str(wln)+" original wave(s) analyzed."
		return ERROR_SUCCESS
	endif

	String wl = srcwl
	String wne =""						//epsc wave name

//	variable i=0
	i = 0
	string pref = epscpref + num2str(twi)+"_*"
	string ewl = wavelist(pref,";","")
	variable ewln = itemsinlist(ewl)
	for(i=0;i<ewln;i+=1)
		wn = StringFromList(i,ewl,";")
		killwave(wn,TRUE,TRUE)
	endfor

	variable pt
	wn = StringFromList(twi,wl,";")			//get single wave name

	wavestats/m=1/q $wn
	pt = v_npnts

	SetSign(wn)

	variable waittime
	variable stimfreq
	variable stimnumber

	wave stimfiber = $"stimfiber"
	if(fmode==0)
		if(twi>0)
			waittime=round(stimfiber[twi-1][0]*1000)
			stimfreq=stimfiber[twi-1][1]
			stimnumber=stimfiber[twi-1][2]
		endif
		do
			prompt waittime,"Time Before First Stimulus[ms]:"
			prompt stimfreq,"Stimulation Frequency[Hz]:"
			prompt stimnumber,"Stimulation Nunber:"
			doprompt "Set Parameters: "+stringfromlist(twi,orgwl),waittime,stimfreq,stimnumber


			if(v_flag==0)
				break
			endif
		while(1)
		stimfiber[twi][0]=waittime/1000
		stimfiber[twi][1]=stimfreq
		wave w = $stringfromlist(twi,srcwl)
		stimfiber[twi][2]=min(stimnumber,floor((rightx(w)+1/sf-leftx(w)-waittime/1000)*stimfreq))

	else
		stimfreq = stimfiber[twi][1]
		stimnumber = stimfiber[twi][2]
	endif

	variable j=sf/stimfreq

	variable k=round(stimfiber[twi][0]*sf)
	variable n=0
	variable b = mean($wn,0,0.9*stimfiber[twi][0])
	wave w = $wn

	w-=b

	for(i=k;i<=pt-j+1;i+=j)

		if(n>=stimnumber)
			break
		endif
		wne=epscpref+num2str(twi)+"_"+num2str(n)

		duplicate/o/r=[i,i+j-1] $wn,$wne
		SetScale/I x,0,1/stimfreq,"s",$wne

		n+=1

	endfor
	RemovePseudo(twi)
	UpdateVersion()
	return ERROR_SUCCESS
End
Override Function RemovePseudo(twi)
											//removes pseudo traces from eEPSC waves.
											//_It finds eEPSC onset starting points and adjusts eEPSC baseline.
											//call: ButtonListener, ShowWave, KillWindows
	variable twi							//trainIndex
											//is used to specify the target train to which the calculation will be
											//_applied. The value could be:
											//  -2    User select.
											//  -1    All trains.
											//  i    The train indexed i.
	string epscpref
	wave/t rn
	epscpref = rn[10][0]

	string wn

	wave stimfiber=$"stimfiber"

	variable/g sf
	if(waveexists($"expinfo")==0)
		return ERROR_INVALID_RES_WAVE
	endif
	wave exw = $"expinfo"
	variable autosettrain = exw[20]
	variable i,j,k,n
	variable pspx
	variable wei
	n=stimfiber[twi][2]

	for(wei=0;wei<n;wei+=1)
		wn = epscpref+num2str(twi)+"_"+num2str(wei)
		wave w = $wn

		if(wei==0)//&&(pseudot[wei]==0))
//	auto judge t0
//			j=0.005*sf
//			for(i=j;i>0;i-=1)
//				if((w[i]<w[i-1])&&(w[i]<-10e-9))
//					break
//				endif
//			endfor

//			differentiate w/d=dw
//			for(;;i-=1)
//				if(dw[i]>dw[i+1])
//					break
//				endif
//			endfor
//			for(;;i-=1)
//				if(dw[i]<dw[i+1])
//					break
//				endif
//			endfor
//			k=i				//EPSC initial time point

//			pseudot[wi] = pspx
//			break
//			return //1
//			print j,k
//			wavestats/q w
//			for(i=j+1;;i+=1)
//				if(w[i]>-v_sdev)
//					break
//				endif
//			endfor
//			j=i
//			k=0

//			for(;;i+=1)
//				if(w[i]>-v_sdev)
//					k+=1
//				else
//					if(k<4)
//						k=0
//					else
//						break
//					endif
//				endif
//			endfor
//			k=(k+mod(k,2))/2+1
//			pspx = (j+k)/sf

	//		print j,k
			if((autosettrain==1)&&(twi!=0))
				stimfiber[twi][3] = stimfiber[0][3]
				stimfiber[twi][4] = stimfiber[0][4]



			else
				showwave(wn,1)
				string wng = wn +"g"
				SetAxis /w=$wng bottom,0,6e-3
				ShowInfo


				DoWindow/F $wng
				Do
					NewPanel/FLT=2/K=2/N=cursorwindow/W=(0,0,200,120) as "Pause for Cursor"
					SetActiveSubwindow _endfloat_
					doupdate/w=cursorwindow
					modifypanel/w=cursorwindow,framestyle=0,noedit=1
				//	DoWindow/W=tmppanel/C cursorwindow // Set to an unlikely name
					AutoPositionWindow/E/M=1/R=$wng // Put panel near the graph

					DrawText 20,20,"Adjust the cursors and then"
					DrawText 50,40,"click Continue."
					Button bstpseudo,pos={60,80},size={80,20},title="Continue",win=cursorwindow
					Button bstpseudo,proc=ButtonListener,win=cursorwindow
					PauseForUser cursorwindow,$wng

					if((pcsr(A)>-1)&&(pcsr(B)>-1))

						stimfiber[twi][4] = min(pcsr(A),pcsr(B))
						stimfiber[twi][3] = max(pcsr(A),pcsr(B))
						killwindows(wng,"",1)
						break
					endif
				While(1)
			endif
		endif
//		print wn

	j = stimfiber[twi][3]
	variable b = stimfiber[twi][4]
	variable base = w[b]
//	j = pspx*sf

//	print j
	for(i=0;i<j;i+=1)
		w[i]=base//+w[j]
	//	print i//
	endfor
	w-=base

//	w-=w[j]
	SetScale/p x,-(j-1)/sf,1/sf,"s",w
//	print pspx
//	SetScale/i x,-pspx,1/stimf[wi]-pspx,"s",w

//	differentiate w/d=dw
//	duplicate/o dw,$"d"+wn
//	SetScale/i x,-v_minloc,1/stimf[wi]-v_minloc,"s",w



	endfor
	return ERROR_SUCCESS
End
Override Function SetTrainConfig(twi)
											//sets train information for eEPSC segmentation.
											//call: GeteEPSC
	variable twi							//trainIndex
											//is used to specify the target train to which the operation will be
											//_applied. The value could be:
											//  -2    User select.
											//  -1    All trains.
											//  i    The train indexed i.

	variable i

	if(waveexists($"expinfo")==0)
		return ERROR_INVALID_RES_WAVE
	endif
	wave exw = $"expinfo"
	variable wln = exw[1]

	wave stimfiber = $"stimfiber"

	variable n = dimsize(stimfiber,0)
	if(twi==SELECT)


		string chstr ="All;"
		string s

		for(i=0;i<n;i+=1)
			s=num2str(i)+": "+num2str(stimfiber[i][1])+"Hz * "+num2str(stimfiber[i][2])+";"
			chstr+=s
		endfor

		variable ch = twi+2
		prompt ch,"Choose Train to Modify:",popup,chstr
		doprompt "Set Train Config",ch
//		doprompt "Average Option",ch
		if(v_flag==0)

			ch-=2

			SetTrainConfig(ch)
		endif

		return ERROR_SUCCESS
	endif

	if(twi==ALL)

		for(i=0;i<wln;i+=1)
			SetTrainConfig(i)
		endfor


		return ERROR_SUCCESS
	endif

	geteepsc(twi,0)
	return ERROR_SUCCESS
End
Override Function GetCaCurrent()
											//gets calcium current waves from original data.
											//details: The calcium currents will be subtracted
											//_by the passive currents. Stimulation protocol
											//_MUST be correctly input or the extracted Ica
											//_waves will be wrong.
											//call: UpdateVersion, CheckSF, SplitCa, SubtractCa, KillWave
	variable/g sf
	if(waveexists($"expinfo")==0)
		return ERROR_INVALID_RES_WAVE
	endif
	wave exw = $"expinfo"
	variable wln = exw[1]

	if(wln==0)
		print "No indexed wave."
		return ERROR_NO_TARGET_WAVE
	endif


	CheckSF()

	variable wi			//wave index


	if(waveexists($"stimca"))
		killwave("stimca",TRUE,TRUE)
	endif

	make/o/n=9 stimca				//is the wave which records the information of presynaptic
									//_stimulation that induces both passive and active calcium
									//_currents.
									//It is an one-dimensional wave (vector). The rows are defined
									//_as:
									//  row 0:    Time before first passive test onset.
									//  row 1:    Base line length, MUST < casubrelax.
									//  row 2:    Passive test pulse length.
									//  row 3:    Passive test relaxation length.
									//  row 4:    Passive test length delta (relax included) between trails.
									//  row 5:    Passive test pulse number.
									//  row 6:    Relaxation between last passive and suprathreshold test onset.
									//  row 7:    Suprathreshold test onset offset.
									//  row 8:    Suprathreshold test pulse number.
									//  row 9:    Holding potential step number.
									//NOTE: The relaxision between the last passive test and first active
									//_stimulus is from the end of the passive test, which means the time point
									//_that last passive relaxion ends, to the suprathreshold onset. It is designed
									//_as the leak subtraction test pulse comes first and the recording pulse
									//_follows. If you used a protocol with a reversed sequence, it also works but
									//_this value should be negative.

	variable cawaitt = 1000		//time before first passive test onset
	variable cabase = 20			//base line length, MUST < casubrelax
	variable casubdura = 52		//passive test pulse length
	variable casubrelax = 100		//passive test relaxation length
	variable casubdelta = 0		//passive test length delta (relax included) between trails
	variable casubn = 10			//passive test pulse number
	variable carelax = 1000		//relaxation between last passive test(relax included) and suprathreshold test onset
	variable cashift = 0			//suprathreshold test onset offset
	variable castimn = 1			//suprathreshold test pulse number
	variable castepn = 0			//holding potential step number

	HideProcedures

	prompt cawaitt,"Waiting Time Before First Substaction Stimulus[ms]:"
	prompt cabase,"Baseline length[ms]:"
	prompt casubdura,"Substraction Stimulation Total Duration[ms]:"
	prompt casubrelax,"Substraction Relax Duration[ms]:"
	prompt casubdelta,"Substraction Total Delta[ms]:"
	prompt casubn,"Substraction Test Pulse Number:"

	prompt carelax,"Relax Duration[ms]:"
//	prompt cashift,"Time Shift Before Recording Stimulus[ms]:"
	prompt castimn,"Recording Stimulation Pulse Number:"
//	prompt castepn,"Step Number:"
//	doprompt "Set Substraction Parameters: ",cawaitt,cabase,casubdura,casubrelax,casubdelta,casubn,carelax,cashift,castimn,castepn
	doprompt "Set Substraction Parameters: ",cawaitt,cabase,casubdura,casubrelax,casubdelta,casubn,carelax,castimn
	if(v_flag==1)
		return ERROR_USER_CANCELED
	endif

	stimca[0] = cawaitt
	stimca[1] = cabase
	stimca[2] = casubdura
	stimca[3] = casubrelax
	stimca[4] = casubdelta
	stimca[5] = casubn
	stimca[6] = carelax
	stimca[7] = cashift
	stimca[8] = castimn

	if(castepn!=0)
		SplitCa(castepn)					//split original wave if more than one holding voltage used
	endif

	for(wi=0;wi<wln;wi+=1)					//make passive current subtraction
		SubtractCa(wi)
	endfor
	UpdateVersion()
	return ERROR_SUCCESS
End
Override Function SplitCa(castepn)
											//splits original data waves if more than one holding potentials
											//_were used during calcium current recording.
											//call: KillWave
	variable castepn						//stepNumber
											//is the number of steps of holding potential used in a trial.

	if((waveexists($"expinfo")==0)||(waveexists($"rchmap")==0))
		return ERROR_INVALID_RES_WAVE
	endif

	wave/t rn,rchmap
	string prewl,prewpref,prewlpref
	variable i = dimsize(rchmap,0)
	if(i==0)
		return ERROR_NO_TARGET_WAVE
	else
		findvalue/text=(rn[11][2])/txop=2 rchmap
		prewl = rchmap[mod(v_value,i)][7]
		prewpref = rchmap[mod(v_value,i)][1]
		prewlpref = prewpref+"*"
	endif

	wave exw = $"expinfo"
	variable wln = exw[1]

	wave stimca = $"stimca"
	variable wi
//	string wl = prewl

//	print wn

	variable/g sf

	variable cawaitt = stimca[0]/1000
	variable casubdura = stimca[2]/1000
	variable casubrelax = stimca[3]/1000
	variable casubn = stimca[5]
	variable carelax = stimca[6]/1000
	variable castimn = stimca[8]
	variable cainter = casubdura+casubrelax

	cainter = round(sf*cainter)
	cawaitt = round(sf*cawaitt)
	carelax = round(sf*carelax)
	variable j = 0
	for(i=0;i<wln-1;i+=2)
		wave suborg = $StringFromList(i,prewl,";")
		wave stimorg = $StringFromList(i+1,prewl,";")
		for(j=0;j<castepn;j+=1)
			duplicate/o/r=[cawaitt+j*cainter*casubn,cawaitt+carelax+(j+1)*cainter*casubn-1] suborg,ssub
			duplicate/o/r=[cawaitt+j*cainter*casubn,cawaitt+carelax+(j+1)*cainter*casubn-1] stimorg,sstim
			concatenate /NP/o {ssub,sstim}, $"splitca"+num2str(j)
			setscale/p x,0,1/sf,"s",$"splitca"+num2str(j)
		//	abort
		endfor
	endfor

	killwave(prewl,TRUE,TRUE)
	prewl = wavelist("splitca*",";","")
	wln = ItemsInList(prewl)
//	print wl,wln
	string wn,wnr
	for(i=0;i<wln;i+=1)
		wn = StringFromList(i,prewl,";")
		wnr = prewpref + num2str(i)					//formatted new name with prefix "org"
//		print "xxxx",wn,wnr
		rename $wn, $wnr
	endfor
	exw[1] =wln
	findvalue/text=(rn[11][2])/txop=2 rchmap
	rchmap[mod(v_value,i)][7] = wavelist(prewlpref,";","")
	return ERROR_SUCCESS
End
Override Function SubtractCa(owi)
											//calculates the passive currents and subtracts them from
											//_calcium current. Stimulation protocol MUST be correctly
											//_input or the extracted Ica waves will be wrong.
											//call: KillWave
	variable owi							//waveIndex
											//is the index of source wave from which the Ica will be
											//_extracted and corrected.


	wave stimca = $"stimca"

	if(waveexists($"rchmap")==0)
		return ERROR_INVALID_RES_WAVE
	endif
	wave/t rn,rchmap
	variable i = dimsize(rchmap,0)
	if(i==0)
		return ERROR_NO_TARGET_WAVE
	else
		findvalue/text=(rn[11][2])/txop=2 rchmap
	endif
	String wl = rchmap[mod(v_value,i)][7]
	String wn = StringFromList(owi,wl,";")
//	print wn

	variable/g sf,datatype



	variable cawaitt = stimca[0]/1000
	variable cabase = stimca[1]/1000
	variable casubdura = stimca[2]/1000
	variable casubrelax = stimca[3]/1000
	variable casubdelta = stimca[4]/1000
	variable casubn = stimca[5]
	variable carelax = stimca[6]/1000
	variable cashift = stimca[7]/1000
	variable castimn = stimca[8]

	variable cainter = casubdura+casubrelax


	wave w = $wn
//	print cawaitt

	variable avg = mean(w,0,cawaitt-cabase)

	w -= avg

	cainter = round(sf*cainter)
	cawaitt = round(sf*cawaitt)
	carelax = round(sf*carelax)
	casubdelta = round(sf*casubdelta)
	cabase = round(sf*cabase)
	cashift = round(sf*cashift)
//	print cainter,cawaitt

	make/o/D/n=(cainter+cabase) $"wtp"			//total passive
	wave wtp
	wtp = 0

	setscale/p x,0,1/sf, "s",wtp
//	print cawaitt,cabase,cainter,casubdelta,owi
	for(i=0;i<casubn;i+=1)
		duplicate/o/r=[cawaitt-cabase+i*(cainter+casubdelta*owi),cawaitt+(i+1)*(cainter+casubdelta*owi)-1] $wn,$"wsp"
//print (cawaitt-cabase+i*(cainter+casubdelta*owi))/sf,(cawaitt-cabase+(i+1)*(cainter+casubdelta*owi)-1)/sf
		wave wsp		//single passive
		setscale/p x,0,1/sf, "s",wsp

		wtp += wsp
//		duplicate/o tmp $"tmp"+num2str(i)
//		duplicate/o tmpca $"tmpca"+num2str(i)
	endfor
//display tmpca;abort


	string capref
	wave/t rn
	capref = rn[11][0]

	if(datatype&2)
		string epscpref
		wave/t rn
		epscpref = rn[10][0]

		string wne = epscpref+num2str(owi)
		i = dimsize(rchmap,0)
		if(i==0)
			return ERROR_NO_TARGET_WAVE
		else
			findvalue/text=(rn[10][2])/txop=2 rchmap
		endif
		string postwl = rchmap[mod(v_value,i)][7]
		string wneo = stringfromlist(owi,postwl,";")
	endif
	for(i=0;i<castimn;i+=1)

		String wnco = "org_"+capref+num2str(owi)
		if (castimn!=1)
			wnco+="_"+num2str(i)
			wne+="_"+num2str(i)
		endif
		duplicate/o/r=[cawaitt-cabase+(casubn+i)*cainter+casubn*owi*casubdelta+carelax+cashift,cawaitt+(casubn+1+i)*cainter+casubn*owi*casubdelta+carelax+cashift-1] $wn,$wnco
		if(datatype&2)
			duplicate/o/r=[cawaitt-cabase+(casubn+i)*cainter+casubn*owi*casubdelta+carelax+cashift,cawaitt+(casubn+1+i)*cainter+casubn*owi*casubdelta+carelax+cashift-1] $wneo,$wne
		endif


//	base(wnco)

		String wncs = capref+num2str(owi)
		if (castimn!=1)
			wncs+="_"+num2str(i)
		endif
		duplicate/o $wnco,$wncs
		wave ca = $wncs
		ca -= wtp
		variable b = mean(ca,leftx(ca),leftx(ca)+cabase/sf)
		ca -= b
		setscale/p x,-(owi*casubdelta+cabase)/sf ,1/sf, "s",$wnco,$wncs
		if(datatype&2)
			setscale/p x,-(owi*casubdelta+cabase)/sf ,1/sf, "s",$wne
		endif
	endfor
	KillWave("wtp;wsp",TRUE,TRUE)
	return ERROR_SUCCESS
End
Override Function ShowAverage()
											//shows averaged waveform of extracted waves.
											//details: Only mEPSC, eEPSC and Ica waves will be calculated
											//_by this operation. To perform averaging on waves with other
											//_type or prefix, use Average operation directly.
											//call: Average

	variable swi,ewi						//startIndex, endIndex
	swi = 0
	ewi = inf
	variable/g datatype

	wave/t rn = $"rn"
	string minipref,epscpref,capref

	minipref = rn[9][2]
	epscpref = rn[10][0]
	capref = rn[11][0]
	string tg = ""
	if(datatype&1)
		tg = lowerstr(rn[0][4]+minipref+"g")
		Average(minipref,swi,ewi)
	endif
	if(datatype&2)
		if(waveexists($"expinfo")==0)
			return ERROR_INVALID_RES_WAVE
		endif
		wave exw = $"expinfo"
		variable wln = exw[1]

		//			string/g postwl

		wave stimfiber = $"stimfiber"

		variable i
		string chstr ="All;"
		string s
		for(i=0;i<wln;i+=1)
			s=num2str(i)+": "+num2str(stimfiber[i][1])+"Hz * "+num2str(stimfiber[i][2])+";"
			chstr+=s
		endfor

		variable ch = 1

		prompt swi,"Start Wave:"
		prompt ewi,"End Wave:"
		prompt ch,"Choose Wave:",popup,chstr
		doprompt "Average Option",ch,swi,ewi
		//		doprompt "Average Option",ch
		if(v_flag==1)
			return ERROR_USER_CANCELED
		endif
		ch-=2



		chstr = epscpref
		if(ch==ALL)
			Average(chstr,swi,ewi)
		else
			//	print swi,ewi
			Average(chstr+num2str(ch)+"_",swi,ewi)
		endif
	endif
	if(datatype&4)

		Average(upperstr(capref[0])+capref[1,inf],swi,ewi)
	//	Average(capref,swi,ewi)
		if(strlen(winlist(tg,";","WIN:1"))!=0)
			AutoPositionWindow/E/M=0/R=$tg
		endif
	endif
	return ERROR_SUCCESS
End
Override Function Average(amode,swi,ewi)
											//makes averaged waveform of target waves by name patterns and indexes.

											//details: The temporal aligned waveform averaging of seleceted target waves
											//_will be performed by this function. The output is a double precision wave
											//_named as "avg_"+amode. If the amode matches with "mEPSC", "mini", or "ca"
											//_(case-insensitive) and the bit 0 of datatype is set (mEPSC data), the time
											//_scale of output wave will be set according to configuration.

											//call: Base, ShowWave, CleanTemp, KillWave, KillWindows, RecHistory
	String amode							//matchStr
											//is used to specify the source waves to do the
											//_waveform average.
	variable swi							//startIndex
											//is used for range-specified average. It is the index of first
											//_wave in the name list which will be used for waveform average.
	variable ewi							//endIndex
											//is the index of last wave selected for the average.

	amode = replacestring("*",amode,"")

	make/o/n=(NumberByKey("N_PARAMS", functioninfo(GetRTStackInfo(1))))/t tmpexhis = {amode,num2str(swi),num2str(ewi)}
	RecHistory()
	ewi+=1

	variable st = ticks


	wave/t rn = $"rn"
	String pref=""							//wave with this prefix will be selected
	if(cmpstr(amode,rn[9][2])==0)			//mEPSC
		pref = rn[9][1]					//mini*
	else									//other waves
		pref = amode+"*"
	endif

	if(cmpstr(pref,rn[9][1])==0)			//use "mEPSC" to build average wave name
		amode = rn[9][2]
	endif
	if(cmpstr(amode,rn[0][0])==0)			//avg_
		amode = "plot"
	endif
	if(strsearch(amode,"_",inf,1)==strlen(amode)-1)		//matchstr ends with underline
		amode=amode[0,strlen(amode)-2]
	endif
	string winn = lowerstr(rn[0][4])+lowerstr(amode)+"g"	//windowname

	killwindows(winn,"",1)										//kill if exist

	string wna = rn[0][0]+lowerstr(amode)						//average wave name
	string wint = rn[0][3]+"_"+amode							//window title
//	print winn
//print amode,rn[0][0]+amode,pref
//

	killwave (wna,TRUE,TRUE)

	String wl = wavelist(pref,";","")
	variable wln = ItemsInList(wl)
	if(wln==0)
		return ERROR_NO_TARGET_WAVE
	endif

	variable wi


	if(waveexists($"expinfo")==0)
		return ERROR_INVALID_RES_WAVE
	endif
	wave exw = $"expinfo"
	variable isdebug = exw[4]
	variable prept = exw[12]
	variable postpt = exw[13]
	variable/g datatype,sf
	string epscpref
	wave/t rn
	variable issdb
	if(isdebug==0)
		execute/z/q "modifybrowser close"
		issdb = v_flag
	else
		execute/z/q "modifybrowser close"
	endif
	epscpref = rn[10][0]
	findvalue/txop=2/text=pref rn
//print pref
	variable len,blen

	switch(mod(v_value,dimsize(rn,0)))
		case 9:
			if (prept+postpt==0)
				print "Not initialized yet. Time window width error."
				return ERROR_NO_TARGET_WAVE
			endif
			len = prept+postpt+1
			blen = prept
			make/o/d/n=(len) wavg = 0
			setscale/p x,-blen/sf,1/sf,"s" wavg
			break
		case 10:

			wave stimfiber = $"stimfiber"

			//	len = sf/(wavemax(stimfiber[1]))
			//else

			len = sf/(stimfiber[str2num(pref[strlen(epscpref),inf])][1])
			blen = stimfiber[str2num(pref[strlen(epscpref),inf])][3]
			//		print len

			//		print len
			//	endif
			make/o/d/n=(len) wavg = 0
			setscale/p x,-blen/sf,1/sf,"s" wavg
			break
		case 11:
			if(datatype&1)
				if (prept+postpt==0)
					print "Not initialized yet. Time window width error."
					return ERROR_NO_TARGET_WAVE
				endif
				len = prept+postpt+1
				blen = prept
				make/o/d/n=(len) wavg = 0
				setscale/p x,-blen/sf,1/sf,"s" wavg
			else

				wave stimca
				len = (stimca[1]+stimca[2]+stimca[3])*sf/1000
				blen = stimca[1]*sf/1000
				//	print "xx",blen,len
				make/o/d/n=(len) wavg
				setscale/p x,-blen/sf,1/sf,"s" wavg
			endif

			break
		default:
			blen = -1
	endswitch


//wavetype
	string wn,wni,wnwg
	if((swi>=wln)||(ewi<0))
		Print "Wrong start and/or end index of target waves."
		return ERROR_INDEX_OUT_OF_RANGE
	endif
	swi = max(swi,0)
	ewi = min(ewi,wln)

	wn = StringFromList(swi,wl)

	if(ewi-swi>500)
		NewPanel/FLT=2/N=ProgressBar/W=(800,560,1200,660)/K=2 as "Waveform Averaging Progress"
		SetActiveSubwindow _endfloat_
		doupdate/w=ProgressBar
		modifypanel/w=ProgressBar,framestyle=0,noedit=1
		ValDisplay prgs,pos={20,55},size={300,18},limits={0,round(1.1*(ewi-swi)),0},barmisc={0,0},frame=0,value= _NUM:0,mode=3,win=ProgressBar
		TitleBox pbtx,pos={20,20},size={300,18},title="Building output wave(s)...",frame=0,win=ProgressBar

		Button bStop,pos={335,55},size={50,20},title="Stop",disable=2,win=ProgressBar
		DoUpdate /W=ProgressBar /E=1	// mark this as our progress window
	endif

	if(blen<0)
		make/o/d/n=(numpnts($wn)) wavg = 0
		setscale/p x,leftx($wn),deltax($wn),waveunits($wn,0) wavg
	endif
	setscale/p y,0,0,waveunits($wn,1) wavg
	if(ewi-swi>500)
		ValDisplay prgs,value= _NUM:round(0.05*(ewi-swi)),win=ProgressBar
		TitleBox pbtx,title="Calculating averaged waveform...",win=ProgressBar
		DoUpdate /W=ProgressBar
	endif

	wint = wint+"_n="+num2str(ewi-swi)

	variable i,j
	for(wi = swi; wi < ewi; wi+=1)
		wn = StringFromList(wi,wl,";")
		duplicate/o $wn,wsrc
	//	redimension/d wsrc
	//	wave tmp = $"tmp"
		if(leftx(wavg)!=leftx(wsrc))

			if(leftx(wavg)<leftx(wsrc))
				i = round((leftx(wsrc)-leftx(wavg))*sf)
				j = wsrc[0]
				insertpoints 0,i,wsrc
				wsrc[0,i]=j
			else
				i = round((leftx(wavg)-leftx(wsrc))*sf)

				deletepoints 0,i,wsrc

			endif

		endif

		wavg += wsrc

		if((mod((wi-swi),100)==0)&&(ewi-swi>500))
			ValDisplay prgs,value= _NUM:round(0.05*(ewi-swi))+wi-swi+1,win=ProgressBar
			TitleBox pbtx,title="Calculating averaged waveform. "+num2str(round(100*(wi-swi)/(ewi-swi)))+"% finished.",win=ProgressBar
			DoUpdate /W=ProgressBar
		endif
	endfor
	string wns = "sum_"+lowerstr(amode)
	duplicate/o wavg,$wns
	wavg /= (ewi-swi)

//	wn = rn[0][3]+"_"+amode
	duplicate/o wavg, $wna
//print ewi-swi
//print "wna",wna
//print waveexists($"wsrc")
//print waveexists($"wavg")
//	KillWave("wsrc;wavg;",TRUE,TRUE)
//		abort
//	rename tmpm $wna
	if(datatype&1)
		Base(wna,x2pnt($wna,0))
	endif

	if(ewi-swi>500)
		ValDisplay prgs,value= _NUM:round(1.2*(ewi-swi)),win=ProgressBar
		TitleBox pbtx,title="Making graph...",win=ProgressBar
		DoUpdate /W=ProgressBar
	endif
//	print itemsinlist(wavelist("*",";",""))
	KillWave("wsrc;wavg;",TRUE,TRUE)
//	print itemsinlist(wavelist("*",";",""))
	ShowWave(wna,1)
//	print itemsinlist(wavelist("*",";",""))
	dowindow/c/t $winn, wint

	dowindow/f $winn

//	print wn,lowerstr(rn[0][0]+amode)
//	rename $wn,$lowerstr(rn[0][0]+amode)

	print num2str(ewi-swi)+" "+amode+" waveform averaging finished. "+num2str((ticks-st)/60)+"s used."
	if(ewi-swi>500)
		ValDisplay prgs,value= _NUM:round(1.2*(ewi-swi)),win=ProgressBar
		TitleBox pbtx,title="Averaging finished.",win=ProgressBar
		DoUpdate /W=ProgressBar

		KillWindow ProgressBar
	endif
	CleanTemp(!isdebug)
	if(isdebug==0)
		if(!issdb)
			execute/z/q "createbrowser"
		endif
	endif
	return ERROR_SUCCESS
End
Override Function Frequency(tsi,tei)
											//returns mEPSC frequency.
											//details: The frequency is calculated as:
											//frequency = mEPSC number / selected time range (in secend)
											//If tsi is set to 0 and tei is set to inf, the procedure will try
											//_to calculate the full length frequency. In this case, if mEPSC extraction
											//_is not finished, the time of the last identified mEPSC will be used
											//_as the denominator. Otherwise, the mEPSC number will be devided by the
											//_total time of original data.
											//Thus, the result may be slightly overestimated if terminates in
											//_the half way, especially when the frequency is low and only few
											//_mEPSC in short period of recording time has been discriminated.
											//_So the more original wave scanned, the better accuracy will be.
											//If the returned value is NaN, it means error occured during the function
											//_execution. Check the error information by looking into the reserved wave
											//_"exerr".

											//call: RecHistory, SetError
	variable tsi							//startTime
											//is the start time point (in secend) for range-specified properties analysis.
	variable tei							//endTime
											//is the end time point (in secend) for range-specified properties analysis.
	string hstr = num2str(tsi)+"_vararg,"+num2str(tei)+"_vararg"

	RecHistory()
	tsi = max(0,tsi)
	variable/g sf
	if(waveexists($"expinfo")==0)
		SetError(ERROR_INVALID_RES_WAVE)
		return NaN
	endif
	wave exw = $"expinfo"

	variable wln = exw[1]
	variable orglength = exw[14]
	variable isgmfin = exw[15]
	wave mntime = $"mntime"
	if(waveexists(mntime)==0)
		SetError(ERROR_NO_TARGET_WAVE)
		return NaN
	endif
	variable n = numpnts(mntime)
	variable minifreq

	if((isgmfin)&&(tsi==0)&&(tei==inf))
		minifreq = n /(wln*orglength/sf)
	else
		tei = min(tei,wln*orglength/sf)

		variable i
		variable ps,pe,ts,te
		for(i=0;i<n;i+=1)
			if(tsi<=mntime[i])
				break
			endif
		endfor
		ps = i
		ts = mntime[ps]
		for(i=n-1;i>=0;i-=1)
			if(tei>=mntime[i])
				break
			endif
		endfor
		pe = i+1
		te = mntime[pe]
		if((pe-ps<0)||(te-ts<0))
			print "Time range error. Start time must not be later than end time."
			SetError(ERROR_INDEX_OUT_OF_RANGE)
			return NaN
		endif
		if(tsi==0)
			ts = 0
		endif
		if(pe-ps==0)
//			print "No wave was found in this time range."
			SetError(ERROR_NO_TARGET_WAVE)
			return NaN
		endif
//		print pe-ps,te-ts,te,ts
		if(isgmfin==0)
			if(ps==0)
				tsi = 0
			else
				tsi = mntime[ps]
			endif
			tei = mntime[pe]
		endif
		minifreq = (pe-ps) /(tei-tsi)
	endif
//	print tei,tsi,ps,pe
	if((minifreq>=0)&&(abs(minifreq)!=inf))
		print "mEPSC frequency = "+num2str(minifreq)+" Hz. Time range: "+num2str(tsi)+"s to " +num2str(tei)+"s."
	else
		minifreq = NaN
	endif
	return minifreq
End
Override Function StatsResult()
											//calculates extacted wave properties.
											//details: It is the entry for menu item. Programmatic
											//_execution of property calculation may skip this operation.
											//call: StatsmEPSC, StatseEPSC
	String minipref,epscpref
	wave/t rn
	minipref = rn[9][1]

	epscpref = rn[10][0]

	variable/g datatype
	if(waveexists($"expinfo")==0)
		return ERROR_INVALID_RES_WAVE
	endif
	wave exw = $"expinfo"
	variable usedefault = exw[0]
	variable wln = exw[1]

	if(datatype&1)
		if((waveexists($"mnname")==0)||(numpnts($"mnname")==0))
	//		print "No mEPSC to analyse."
			return ERROR_NO_TARGET_WAVE
		else


			variable ts,te					//time range
			ts = 0
			te = inf
			if(usedefault!=1)
	//		print usedefault
				prompt ts,"Start time[s]:"
				prompt te,"End time[s]:"
				doprompt "Choose time range",ts,te
				if (v_flag==1)
					return ERROR_USER_CANCELED
				endif
			endif
			StatsmEPSC(ts,te)

		endif
	endif
	if(datatype&2)
//		string/g postwl

		wave stimfiber = $"stimfiber"
		variable n = numpnts(stimfiber)/5
		variable i
		string chstr ="All;"
		string s
		for(i=0;i<n;i+=1)
			s=num2str(i)+": "+num2str(stimfiber[i][1])+"Hz * "+num2str(stimfiber[i][2])+";"
			chstr+=s
		endfor

		variable ch = 1
		prompt ch,"Choose Wave:",popup,chstr
		doprompt "Stats Option",ch
		if(v_flag==0)
			ch-=2
			chstr = epscpref
			if(ch==ALL)
				StatseEPSC(chstr)

			else
				StatseEPSC(chstr+num2str(ch)+"_")
			endif
		endif
	endif
	return ERROR_SUCCESS
End
Override Function StatsmEPSC(tsi,tei)
											//calculates mEPSC wave properties.
											//details: The amplitude, rise time, total charge, half-peak width,
											//_tau1, tau2, a1 and a2 of target waves will be calculated and statistical
											//_results will be displayed on tables and graphs.
											//NOTE: Multithread calculation is used during the operation execution. DO
											//_NOT terminate the procedure in case the loss of data unless very necessary.

											//call: UpdateVersion, PrepareData, GetPropertyMT, ShowStats, CleanTemp, RecHistory
	variable tsi							//startTime
											//is the start TIME (in second) point from where the mEPSCs are considered
											//_in the time-specified property calculation.
	variable tei							//endTime
											//is the end TIME (in second) point to where the mEPSCs are considered
											//_in the time-specified property calculation.

	make/o/n=(NumberByKey("N_PARAMS", functioninfo(GetRTStackInfo(1))))/t tmpexhis = {num2str(tsi),num2str(tei)}
	RecHistory()
	setdatafolder root:
	wave/t mnname = $"mnname"
	wave mntime = $"mntime"
	wave/t rn = $"rn"
	variable n = numpnts(mnname)
	if (n==0)
		//		print "No mEPSC to analyse"
		return ERROR_NO_TARGET_WAVE
	endif

	string wn

	variable wi,i,isdebug,ps,pe
	if(waveexists($"expinfo")==0)
		isdebug = FALSE
	else
		wave exw = $"expinfo"
		isdebug = exw[4]
	endif

	variable issdb
	if(isdebug==0)
		execute/z/q "modifybrowser close"
		issdb = v_flag
	endif
	variable/g ncore
	if(ncore<=0)
		ncore = ThreadProcessorCount
	endif
	for(i=0;i<n;i+=1)
		if(tsi<=mntime[i])
			break
		endif
	endfor
	ps = i

	for(i=n-1;i>=0;i-=1)
		if(tei>=mntime[i])
			break
		endif
	endfor
	pe = i+1

	if(pe-ps<0)
		print "Time range error. Start time must not be later than end time."
		return ERROR_INDEX_OUT_OF_RANGE
	endif
	if(pe-ps==0)
		if((tsi!=0)||(tei!=inf))
			print "No wave was found in this time range."
			return ERROR_NO_TARGET_WAVE
		else
			ps = 0
			pe = n
		endif
	endif

	string suffix
	if(pe-ps==n)
		suffix = ""
	else
		suffix = num2str(tsi)+"_"+num2str(tei)
	endif
	//	print "suffix=",suffix
	string wl = winlist("*",";","WIN:7")
	variable m = itemsinlist(wl)
	for(i=0;i<m;i+=1)
		wn = stringfromlist(i,wl,";")
		dowindow/HIDE=1 $wn
	endfor
	HideProcedures
	//	print "1"
	//	for (i=1;i<9;i+=1)
	//		make/o/n=(pe-ps) $rn[i][0]+suffix = 0
	//	endfor
	//	make/n=0/o/t bn		//bad mepsc name wave
	variable st
	st = ticks
	if(pe-ps>2000)
		NewPanel/FLT=2/N=ProgressBar /W=(800,560,1200,660)/K=2 as "Properties Calculation Progress"
		SetActiveSubwindow _endfloat_
		doupdate/w=ProgressBar
		modifypanel/w=ProgressBar,framestyle=0,noedit=1

		ValDisplay prgs,pos={20,55},size={300,18},limits={0,2*(pe-ps),0},barmisc={0,0},frame=0,value= _NUM:0,mode=3,win=ProgressBar
		TitleBox pbtx,pos={20,20},size={300,18},title="Preparing waves for calculation...",frame=0,win=ProgressBar

		Button bStop,pos={335,55},size={50,20},title="Stop",disable=2,win=ProgressBar
		DoUpdate /W=ProgressBar/E=1	// mark this as our progress window
	endif
	variable rnd
	rnd = PrepareData(suffix,ps,pe,TRUE)
	if(pe-ps>2000)
		ValDisplay prgs,value= _NUM:round(0.4*(pe-ps)),win=ProgressBar
		DoUpdate /W=ProgressBar
	endif
	m = ncore * rnd
	//	print "1",(ticks-st)/60
	//	abort
	//	wave mtvargu
	variable pid = ThreadGroupCreate(ncore)
	//		variable nc = ncore
	//		newdatafolder root:tmpfd
	//		movewave mtsargu, root:tmpfd
	//		movewave mtprop, root:tmpfd
	//		movewave mtvargu, root:tmpfd
	//		setdatafolder root:tmpfd
	for(i=0;i<ncore;i+=1)
		//	print pid,i,mtsargu
		ThreadStart pid, i, GetPropertyMT(rnd)
	endfor
	string dfpref = "tmpdfmt"
	string dfn = ""
	for(i=0;i<m;i+=1)
		dfn = dfpref+num2str(i)
		//	newdatafolder/o root:$dfn
		ThreadGroupPutDF pid,root:$dfn
	endfor

	i=0
	do
		DFREF dfr= ThreadGroupGetDFR(pid,100)

		if ( DatafolderRefStatus(dfr) == 0 )
			//			Print "Main still waiting for worker thread results"
		else
			MoveDataFolder dfr,root:
			i+=1
			if(pe-ps>2000)
				ValDisplay prgs,value= _NUM:round((0.4+i/m)*(pe-ps)),win=ProgressBar
				TitleBox pbtx,title="Calculating properties. "+num2str(round(100*i/m))+"% finished.",win=ProgressBar
				DoUpdate /W=ProgressBar
			endif
			//		break
		endif

	while(i<m)
//do
//	endfor
	if(ThreadGroupRelease(pid))
		print "Some threads are still running. Force quit is needed. Thread Group ID:"+num2str(pid)+"."
	endif
//			else
//			do
//			DFREF dfr= ThreadGroupGetDFR(pid,1000)
//
//			if (DatafolderRefStatus(dfr)==0)
//	//			Print "Main still waiting for worker thread results"
//			else
//				MoveDataFolder dfr,root:
//				i+=1
//				break
//			endif
//
//		while(i<ncore)
//	//		if(i==6)
//			break
//			endif

//	print "6",(ticks-st)/60
//	while(1)
//	abort
	//		movewave mtsargu, root:
	//		movewave mtprop, root:
	//		movewave mtvargu, root:
	//		setdatafolder root:
	//	killwave("tmpm*",1,1)
//	duplicate/o/r=[][0] mtprop amp
//	duplicate/o/r=[][1] mtprop tc
//	duplicate/o/r=[][2] mtprop rt
//	duplicate/o/r=[][3] mtprop hw
//	duplicate/o/r=[][4] mtprop tau1
//	duplicate/o/r=[][5] mtprop tau2
//	duplicate/o/r=[][6] mtprop a1
//	duplicate/o/r=[][7] mtprop a2
//	redimension/n=(dimsize(amp,0)) amp,tc,rt,hw,tau1,tau2,a1,a2
//	if(1)
	if(pe-ps>2000)
		ValDisplay prgs,value= _NUM:round(1.4*(pe-ps)),win=ProgressBar
		TitleBox pbtx,title="Building output waves...",win=ProgressBar
		DoUpdate /W=ProgressBar
	endif
	PrepareData(suffix,ps,pe,FALSE)
//	endif

	wave amp = $rn[1][0]+suffix
	wave tc = $rn[2][0]+suffix
	wave rt = $rn[3][0]+suffix
	wave hw = $rn[4][0]+suffix
	wave tau1 = $rn[5][0]+suffix
	wave tau2 = $rn[6][0]+suffix
	wave a1 = $rn[7][0]+suffix
	wave a2 = $rn[8][0]+suffix


//	endif

	st = ticks - st
	st=st/60
	if(pe-ps>2000)
		ValDisplay prgs,value= _NUM:round(1.9*(pe-ps)),win=ProgressBar
		DoUpdate /W=ProgressBar
	endif
	if(pe-ps==n)
		print num2str(pe-ps)+" mEPSC analyzed. "+num2str(st)+"s used."
	else
		print num2str(pe-ps)+" mEPSC from " +num2str(tsi) + "s to " + num2str(tei) + "s analyzed. "+num2str(st)+"s used."
	endif

//
//
//	if (waveexists(amp))
//		amp = abs(amp)
//	endif
//	if (waveexists(tc))
//		tc = abs(tc)
//	endif
//	print suffix
	ShowStats(suffix)
	if(pe-ps>2000)
		TitleBox pbtx,title="Task finished.",win=ProgressBar
		DoUpdate /W=ProgressBar
		KillWindow ProgressBar
	endif
	if(isdebug==0)
		if(!issdb)
			execute/z/q "createbrowser"
		endif
	endif
	CleanTemp(!isdebug)
	UpdateVersion()
	return ERROR_SUCCESS
End
Override Function PrepareData(psfx,swi,ewi,pmode)
							//returns the number of loops that each thread will do
							//_during a multithread task.
							//call: FolderList, KillWindows
							//seealso: ThreadSafe Functions, ThreadSafe Functions and Multitasking
	string psfx				//affixStr
							//is used to select target waves.
	variable swi			//startWaveIndex
							//is the start index of name of target waves. Only waves with indexes no smaller than
							//_swi will be used during data analyzing.
	variable ewi			//endWaveIndex
							//is the end index of name of target waves. Only waves indexed smaller than
							//_ewi will be used during data analyzing.
	variable pmode			//prepMode
							//is the mode for data preparing process.
	string cfn = GetRTStackInfo(2)
	string wl = ""
	wave/t rn,mnname
	variable i,j,m,n,ps,pe,k,len
	setdatafolder root:
	string dfpref = ""
	string dfn = ""
	n = ewi - swi
	variable rnd = 0
	variable subn = 0
	variable sublen = 0
	variable/g datatype,sf,ncore
	variable ldt = datatype
	variable lsf = sf
	rnd = ceil(n/1000/ncore)			//round for execution
	subn = rnd*ncore					//sub group number
	wave/t rn,rnw
	string wn
	switch(pmode)
		case TRUE:
			//		make/o/n=(5) mtsargu						//system arguments

	//		print swi,ewi,rnd,ncore
			killwindows(rnw[0],"",1)
			killwindows("*",rn[9][1],1)
			strswitch(cfn)
				case "StatsmEPSC":			//called by statsmepsc (property)
			//		string suffix = wn
					dfpref = "tmpdfmt"
					wl = ""

					for(i=0;i<subn;i+=1)
						dfn = dfpref+num2str(i)
						if(datafolderexists(dfn))
							killdatafolder $dfn
						endif
						newdatafolder $dfn
					endfor
					//	m = ceil((pe-ps)/k)
					//	make/o/n=0/t sl
					//	make mtsl
					//			make/o/n=(subn)/t mtsl = ""
					//			for(i=ps;i<pe;i+=1)
					//				wl += mnname[i]+";"
					//				mtsl[floor((i-ps)/k)] += mnname[i]+";"
					//			endfor

					for(i=0;i<rnd;i+=1)
						sublen = min(ceil(ewi/ncore-i*1000),1000)
						for(j=0;j<ncore;j+=1)
							dfn = dfpref+num2str(i*ncore+j)
							dfref dfr = $(getdatafolder(1)+dfn)
							ps = i*1000*ncore + j*sublen
							pe = min(i*1000*ncore + (j+1)*sublen,ewi)
							//	print $dfr
							//	print i,waveexists($"mtsl"),waveexists($("mtsl"+num2str(i*ncore+j)))
							if(pe-ps>0)
								duplicate/r=[ps,pe-1]/t mnname,$"mtsl"
							else
								make/o/n=0 $"mtsl"
							endif
							wave/t mtsl = $"mtsl"
							len = numpnts(mtsl)

							for(k=0;k<len;k+=1)
								movewave $mtsl[k],dfr
							endfor
							movewave $"mtsl",dfr:mnname

							movevariable datatype,dfr
							movevariable sf,dfr
							variable/g datatype = ldt
							variable/g sf = lsf
						endfor
					endfor
					break
				default:
			endswitch
		//	abort
			break

		case FALSE:
			strswitch (cfn)
				case "StatsmEPSC":
					dfpref = "tmpdfmt"
					string pl = ""
					for(i=1;i<9;i+=1)
						pl += rn[i][0] +";"
					endfor
				//	print pl
					string minipref = rn[9][1]
					string fl = FolderList("",dfpref+"*")
//					i = strsearch(fl,",",0)
//					if(i<0)
//						return //-1
//					endif
			//		print fl
//					i = strsearch(fl,":",0)
//					fl = replacestring(",",fl[i+1,strlen(fl)-2],";")
					n = itemsinlist(fl)
				//	print fl,n
			//		print pl
					if(strlen(winlist("ProgressBar",";","WIN:64")))
						ControlInfo/w=ProgressBar prgs
						variable prov = v_value
					endif
					for(i=0;i<n;i+=1)
						dfn = stringfromlist(i,fl)
					//	print dfn
						if(grepstring(dfn,dfpref+"\d+")==0)
							continue
						endif


						setdatafolder $dfn
						wl = wavelist(minipref,";","")
						m = itemsinlist(wl)

						for(j=0;j<m;j+=1)
							wn = stringfromlist(j,wl)
							movewave $wn,root:
						endfor
						for(j=0;j<8;j+=1)
							wn = stringfromlist(j,pl)
					//		print dfn,wn,(dfpref+wn+dfn[strlen(dfpref),inf])
							movewave $wn,root:$(dfpref+wn+dfn[strlen(dfpref),inf])
						endfor
				//		print "xxxx",getdatafolder(1),dfn,m
						setdatafolder root:
						killdatafolder $dfn

						if(strlen(winlist("ProgressBar",";","WIN:64")))
							ValDisplay prgs,value= _NUM:round(prov+i/n*prov*5/14),win=ProgressBar
							TitleBox pbtx,title="Processing backup waves. "+num2str(round(100*i/n))+"% finished.",win=ProgressBar
							DoUpdate /W=ProgressBar
						endif
					endfor
					for(j=1;j<9;j+=1)
						wl = sortlist(wavelist(dfpref+rn[j][1],";",""),";",16)
				//		print wl
				//		edit $stringfromlist(0,wl)
				//		abort
						concatenate/kill/np/o wl,$rn[j][0]+psfx
					endfor
//					ShowWave(rnw[0],5)
//					dowindow/c/t $(lowerstr(rnw[0])) rnw[0]
//					wave/t mnname
//					n=numpnts(mnname)
//					for(i=0;i<n;i+=1)
//						wn = mnname[i]
//						appendtograph/w = $(lowerstr(rnw[0])) $wn
//					endfor
//					dowindow/hide=1 $(lowerstr(rnw[0]))
					break
				default:
			endswitch

			//
			//
			//			k = numpnts($mnname[ps])
			//
			//			string subwn = ""
			//
			//			string dfn
			//			dfref df
			//			string swl
			//			variable j,len
			//			dfpref = "tmpdfp"
			//			//	print m
			//			for(i=0;i<m;i+=1)
			//				setdatafolder root:
			//
			//				dfn = dfpref +num2str(i)
			//				newdatafolder root:$dfn
			//				df = root:$dfn
			//				swl = sl[i]
			//				len = itemsinlist(swl)
			//				//	print len
			//				for(j=0;j<len;j+=1)
			//					//	movewave $stringfromlist(j,swl),df
			//					wn = stringfromlist(j,swl)
			//					//	duplicate/o $wn,df:$wn
			//					movewave $wn,df
			//				endfor
			//				setdatafolder df
			//				subwn = "submtvargu" + num2str(i)
			//				make/o/n=0 $subwn = 0
			//				//	//		wave submtvargu = $("submtvargu"+num2str(i))
			//				//	//		wls =
			//				redimension/n=(k,len) $subwn
			//				//			string cl = sl[i]
			//				//		if(pe-ps>3000)
			//				//	save/o/b/t/p=$path sl[i] as "tmp"+num2str(i)
			//				//		concatenate/o/kill sl[i],$subwn
			//				//	else
			//				//			concatenate/o/kill swl,$subwn
			//				concatenate/o swl,$subwn
			//				movewave $subwn,root:
			//
			//				//	endif
			//				//			duplicate/o submtvargu,$("submtvargu"+num2str(i))
			//				//		print i,(ticks-st)/60
			//				//			st = ticks
			//			endfor
			//			setdatafolder root:
			//			//	endfor
			//			//	print "4.5",(ticks-st)/60
			//			//	abort
			//			make/o/n=0 mtvargu = 0
			//			redimension/n=(k,pe-ps) mtvargu
			//			swl = wavelist("submtvargu*",";","")
			//			concatenate/o/np=1/kill swl,mtvargu
			//			//	concatenate/o/np=1 wavelist("submtvargu*",";",""),mtvargu
			//			//	killwave("submtvargu*",FALSE,TRUE)
			//		elseif(2)
			//		endif
			//	elseif(pmode==FALSE)
			//		for(i=0;i<m;i+=1)
			//			//	swl = sl[i]
			//			dfn = dfpref +num2str(i)
			//			dfref df = root:$dfn
			//			setdatafolder df
			//			swl = wavelist("*",";","")
			//			len = itemsinlist(swl)
			//			for(j=0;j<len;j+=1)
			//				movewave $stringfromlist(j,swl),root:
			//			endfor
			//			setdatafolder root:
			//			killdatafolder df
			//			//			print i,(ticks-st)/60
			//			//		st = ticks
			//		endfor
			break
		default:
	endswitch
	return rnd
End
Override Function StatseEPSC(tpref)
											//calculates eEPSC wave properties.
											//details: The amplitude, rise time, total charge, half-peak width,
											//_tau1, tau2, a1 and a2 of target waves will be calculated.

											//call: UpdateVersion, GetAmplitude, GetRiseTime, GetHalfWidth, GetDecay, GetTotalCharge, ShowStats, ConcatWave, CleanTemp
	string tpref							//wavePrefix
											//is the prefix used to select the target eEPSC waves.
	variable swi,ewi
	string epscpref
	wave/t rn
	epscpref = rn[10][0]


	string suffix = tpref[strlen(epscpref),inf]

//	print suffix
//	print pref
//	print itemsinlist(pref,"_")
//	print pref[0,strlen(pref)-strlen(suffix)]

//				print strsearch(suffix,"_",0)
//			print max(0,str2num(suffix[strsearch(suffix,"_",0)+1,inf]))
//			print strsearch(suffix,"_",inf,1)
//			print min(inf,str2num(suffix[strsearch(suffix,"_",inf,1)+1,inf])+1)
//	abort
	variable i
//	print pref
//	print strlen(epscpref)-1
//	print suffix
	variable wi,ewln
	string wl,wn
	if(waveexists($"expinfo")==0)
		return ERROR_INVALID_RES_WAVE
	endif
	wave exw = $"expinfo"
	variable wln = exw[1]
	variable dstats = exw[2]
	variable isdebug = exw[4]
	if(cmpstr(suffix,"")==0)
		exw[2] = FALSE
		for(i = 0;i<wln;i+=1)
			StatseEPSC(tpref+num2str(i)+"_")
		endfor
		exw[2] = dstats
		for(i=1;i<9;i+=1)
			wl = greplist(wavelist(rn[i][1],";",""),"^"+rn[i][0]+"\d")
			//	print wl
			if(ItemsInList(wl)!=0)
				ConcatWave(wl,0,TRUE,TRUE,rn[i][0])
			endif

		endfor
//		print suffix
		ShowStats(suffix)
		return ERROR_SUCCESS
	else
//	print "xx",tpref
		if(waveexists($tpref+"0")==0)
//		print suffix
//		print pref

			if(itemsinlist(tpref,"_")!=3)
	//			print -1
				return ERROR_INDEX_OUT_OF_RANGE
			endif
		endif
		if(itemsinlist(tpref,"_")==3)
	//		print suffix
			tpref = tpref[0,strlen(tpref)-strlen(suffix)]

		endif
		tpref=tpref+"*"


		wl = wavelist(tpref,";","")
		ewln = ItemsInList(wl)

		if(itemsinlist(suffix,"_")==3)
	//		print suffix
			variable sp
			sp = strsearch(suffix,"_",0)
			swi = max(0,str2num(suffix[sp+1,inf]))
			sp = strsearch(suffix,"_",inf,1)
			ewi = min(ewln,str2num(suffix[sp+1,inf])+1)
		else
			if (strlen(suffix)>1)
				suffix=suffix[0,strlen(suffix)-2]
			endif
			swi = 0
			ewi = ewln
		endif

		wave/t rn = $"rn"
		for (i=1;i<9;i+=1)
			make/o/n=(ewi-swi) $rn[i][0]+suffix

		endfor

		wave amp = $rn[1][0]+suffix
		wave tc = $rn[2][0]+suffix
		wave rt = $rn[3][0]+suffix
		wave hw = $rn[4][0]+suffix
		wave tau1 = $rn[5][0]+suffix
		wave tau2 = $rn[6][0]+suffix
		wave a1 = $rn[7][0]+suffix
		wave a2 = $rn[8][0]+suffix
	//	print tpref,swi,ewi
		for(wi=swi;wi<ewi;wi+=1)

			wn = StringFromList(wi,wl,";")
			wave stimfiber =$"stimfiber"
			variable wei
			wei = str2num(wn[strlen(epscpref),inf])
//		print wei
			if(stimfiber[wei][1]<20)
				duplicate/o/r=(0,0.5/stimfiber[wei][1]) $wn,tmp
		//	wavestats tmp

			else
				duplicate/o/r=(0,inf) $wn,tmp
			endif

			tmp-=tmp[0]
			amp[wi-swi] = GetAmplitude("tmp")
			tc[wi-swi] = GetTotalCharge("tmp")
			rt[wi-swi] = GetRiseTime("tmp")
			hw[wi-swi] = GetHalfWidth("tmp")


			wave w = GetDecay("tmp")
			a1[wi-swi] = w[1]
			tau1[wi-swi] = w[2]
			a2[wi-swi] = w[3]
			tau2[wi-swi] = w[4]
		endfor


		if (waveexists(amp))
			amp = abs(amp)
		endif
		if (waveexists(tc))
			tc = abs(tc)
		endif
	endif
	ShowStats(suffix)
	CleanTemp(!isdebug)
	UpdateVersion()
	return ERROR_SUCCESS
End
Threadsafe Override Function GetPropertyMT(rnd)
								//is the executor of multithread-based property calculation
								//_process.
								//call: GetAmplitude, GetRiseTime, GetHalfWidth, GetDecay, GetTotalCharge
								//seealso: ThreadSafe Functions, ThreadSafe Functions and Multitasking
	variable rnd				//taskRound
								//is the number decides how many loops the task should do.
	variable rx
	for(rx=0;rx<rnd;rx+=1)
	do
		string df = ThreadGroupGetDF(0,100)
		//		print "MT df=",df
		if(strlen(df)==0)
			if(GetRTError(2))	// New in 6.2 to allow this distinction:
				Print "worker closing down due to group release"
//			else
//				Print "worker thread still waiting for input queue"
			endif
		else
			break
		endif
	while(1)

	setdatafolder df
//	ThreadGroupPutDF 0,:
//	return //1
	wave/t mnname

	variable i,n
//	wave mtsargu

	//,mtprop,mtvargu
////	dfref df = ThreadGroupGetDFR(0, 0)
////	print "DataFolderRefStatus(df)",DataFolderRefStatus(df)
////	setdatafolder df
////	print "numpnts($amp)",numpnts($"amp")
////	print "wl="+wavelist("*",";","")
//	variable ps,pe
//
//	ps = mtsargu[0]
//	pe = mtsargu[1]
	nvar/sdfr=df datatype
//	datatype = mtsargu[2]
	nvar/sdfr=df sf
//	sf = mtsargu[3]
//	print datatype,sf

//	variable i
//	n = dimsize(mtprop,0)
//	print "n=",n,"ps=",ps,"pe=",pe,"thoff=",thoff
////	print dimsize(mtvargu,0)
////	print dimsize(mtvargu,1)
	make/t/n=0/o bn
	string wn

//	waveclear mnname,mtsargu,bn
//	ThreadGroupPutDF 0,:
//	return //1
//
//	variable ks = 0
//	variable st = ticks
//variable i
	n = numpnts(mnname)
	make/n=(n) amp,tc,rt,hw,tau1,tau2,a1,a2

//	waveclear mnname,mtsargu,bn
//	waveclear amp,tc,rt,hw,tau1,tau2,a1,a2
//	ThreadGroupPutDF 0,:
//	return //1
//	return //1
	for(i=0;i<n;i+=1)
		wn = mnname[i]

//		print i//,",wn=",wn
//		duplicate/r=[][i] mtvargu $wn
//		redimension/n=(numpnts($wn)) $wn
//		if(waveexists($wn))
//			print wi,wn,ps,pe,itemsinlist(nstr),tid
//		endif

		amp[i] = GetAmplitude(wn)				//amp
		tc[i] = GetTotalCharge(wn)			//tc
//
		rt[i] = GetRiseTime(wn)				//rt
//
		hw[i] = GetHalfWidth(wn)				//hw
//	if(0)
		wave w = GetDecay(wn)
		tau1[i] = w[2]							//tau1
		tau2[i] = w[4]							//tau2
		a1[i] = w[1]							//a1
		a2[i] = w[3]							//a2
//
//	//	print w
//		endif
//		ks +=1
//		if(mod(ks,1000)==0)
//			print tid,ks,(ticks-st)/60
//		endif
	endfor

	waveclear mnname,bn
	waveclear amp,tc,rt,hw,tau1,tau2,a1,a2,w
	ThreadGroupPutDF 0,:
	endfor
//	return //0
//	print tid,ks
//	print "lasti",i
//	return //1
End
Threadsafe Override Function GetAmplitude(wn)
											//returns the amplitude (peak) value of specified wave.
	String wn								//waveName
											//is the name of target wave.
	variable/g pta							//is a temporary global variable which holds the amplitude value
											//_that is latest calculated. It is used as a basic
											//_parameter in other properties analysis and will be used
											//_by the corresponding functions. The performance will
											//_be improved by this way.
	duplicate/o $wn, $"tmpm"				//tmp wave
	wave tmpm
//	if(waveexists(tmpm))
//		print "amp",wn,numpnts(tmpm)
//	endif
	variable n = sign(mean($wn,0,1e-3))	//sign of the wave
	variable/g expsign
	if(n==-expsign)
		n = expsign
	endif
	tmpm = tmpm*n

	pta = wavemax(tmpm)
	return pta								//return value for single wave
End
Threadsafe Override Function GetRiseTime(wn)
											//returns rise time of the wave named wn.
											//call: GetAmplitude
	String wn								//waveName
											//is the name of target wave.
	wave tmpm

	variable/g datatype,sf,pta
	variable r

	if((pta==0)||(waveexists(tmpm)==0))
		GetAmplitude(wn)
		wave tmpm
	endif
	Variable rs,re,rl,rh,i,n
	if (datatype&1)
		rl=0.2 * pta
		rh=0.8 * pta
	else
		rl=0.1 * pta
		rh=0.9 * pta
	endif
	n = numpnts(tmpm)

	for(i=0;i<n;i+=1)
		if(tmpm[i]>=rh)
			break
		endif
	endfor
	re=min(i,n-1)
	for(;i>0;i-=1)
		if(tmpm[i-1]<rl)
			break
		endif
	endfor
	rs=max(i,1)

	rs = rs-1+abs((tmpm[rs-1]-rl)/(tmpm[rs]-tmpm[rs-1]))
	re = re-1+abs((tmpm[re-1]-rh)/(tmpm[re]-tmpm[re-1]))

//	if (GetRTError(0))
//		print GetErrMessage(GetRTError(2)),"RT",wn
//	endif
//	print rs,re
	if(re-rs>0)
		r = (re-rs)/sf
	else
		r = NaN
	endif
	return r						//return value for single wave.

End
Threadsafe Override Function GetHalfWidth(wn)
											//returns half width of the wave named wn.
											//call: GetAmplitude
	String wn								//waveName
											//is the name of target wave.
	wave tmpm

	variable/g sf,pta
	if((pta==0)||(waveexists(tmpm)==0))
		GetAmplitude(wn)
		wave tmpm
	endif
	Variable hs,he,hf,pt,i,n,r

	hf = 0.5*pta
	wavestats/q/m=1 tmpm
	pt = x2pnt(tmpm,v_maxloc)

	for(i=0;i<pt;i+=1)
		if(tmpm[i]>=hf)
			break
		endif
	endfor
	hs = max(i,1)
	n = numpnts(tmpm)
	for(i=pt;i<n;i+=1)
		if(tmpm[i]<=hf)
			break
		endif
	endfor
	he=min(i,n-1)

	hs = hs-1+abs((tmpm[hs-1]-hf)/(tmpm[hs]-tmpm[hs-1]))
	he = he-1+abs((tmpm[he-1]-hf)/(tmpm[he]-tmpm[he-1]))
//	if (GetRTError(0))
//		print GetErrMessage(GetRTError(2)),"RT",wn
//	endif
	if(he-hs>0)
		r = (he-hs)/sf
	else
		r = NaN
	endif
	return r					//return value for single wave
End
Threadsafe Override Function/WAVE GetDecay(wn)
											//gets decay tau1, tau2, a1, a2 of the wave named wn.
											//call: GetAmplitude
	String wn								//waveName
											//is the name of target wave.
	wave tmpm// = $"tmpm"

	variable/g datatype,pta
//	print "fit",waveexists(tmpm),pta
	if((pta==0)||(waveexists(tmpm)==0))
	//	print "xxx"
		GetAmplitude(wn)
		wave tmpm
	endif
//	print tmpm
	wavestats/q/m=1 tmpm
//	if(cmpstr("mini40_0",wn)==0)
//		print tmpm,v_maxloc,rightx(tmpm)
//	endif
//	print deltax(tmpm)
//	print v_npnts,v_maxloc
//		print numpnts(tmpm),wn,v_maxloc,rightx(tmpm)
	variable V_FitError

//	CurveFit/NTHR=0/Q/N/W=2 dblexp,tmpm(v_maxloc,rightx(tmpm))
//	make/o/n=5 w_coef
//	duplicate/o tmpm tmpm1
//	print deltax(tmpm1),v_maxloc,rightx(tmpm1)
	CurveFit/NTHR=0/Q/N/W=2 dblexp,tmpm[x2pnt(tmpm,v_maxloc),inf]
	if(v_fiterror!=0)
//		if (datatype==2)
//			string epscpref
//			wave/t rn
//			epscpref = rn[10][0]
//
//			//	wn = epscpref[0,strlen(epscpref)-2]+sfx+"_"+num2str(wi)
//		else
//			wave/t bn = $"bn"
//		//	insertpoints numpnts(bn),1,bn
//		//	bn[numpnts(bn)-1] = wn
//		endif
//		print wn+" Fit Error. ErrorCode:",v_fiterror
//print geterrmessage(getrterror(1))
////		print "xx",v_fiterror,wn
//		print numpnts($"w_coef")
	endif
//
//
	wave w = w_coef
//	nvar k1,k2,k3,k4
//	print w,k1,k2,k3,k4
//	print "yy",numpnts($"w_coef")
	variable tau1,tau2,a1,a2
	variable dc1,dc2,dc3,dc4
	dc1 = w[1]
	dc2 = w[2]
	dc3 = w[3]
	dc4 = w[4]
	if(dc2>dc4)
		tau1 = 1/dc2
		tau2 = 1/dc4
		a1 = dc1
		a2 = dc3
	else
		tau1 = 1/dc4
		tau2 = 1/dc2
		a1 = dc3
		a2 = dc1
	endif
	if(tau1>0)
		w[2] = tau1
	else
		w[2] = NaN
	endif
	if(tau2>0)
		w[4] = tau2
	else
		w[4] = NaN
	endif
	if(a1>0)
		w[1] = a1
	else
		w[1] = NaN
	endif
	if(a2>0)
		w[3] = a2
	else
		w[3] = NaN
	endif
	tau1 = getrterror(1)
//print w
	return w
End
Threadsafe Override Function GetTotalCharge(wn)
											//returns total charge of the wave named wn.
											//details: When calculating the total charge, the function
											//call: GetAmplitude
	String wn								//waveName
											//is the name of target wave.
	wave tmpm
	variable/g pta
	if((pta==0)||(waveexists(tmpm)==0))
		GetAmplitude(wn)
		wave tmpm
	endif
	variable/g datatype
	integrate tmpm /d=charge
	variable c
	if(datatype&2)
		variable tailx = NumVarOrDefault("root:ectail",20)
		if(tailx==0)
			tailx = 20
		endif
		tailx /= 1000
		c = charge(tailx)
	else
		if(datatype&1)
			c = wavemax(charge)
		endif
	endif


	return c								//return value for single wave

End
Override Function ShowStats(sfx)
											//shows statistical results of properties.
											//details: The mean values, standard division (S.D.)
											//_and standard error (S.E.M.) of the calculated properties
											//_will be shown in a table. While the six major properties
											//_will be displayed in graphs.
											//call: ShowWave, ShowTable, KillWindows
	string sfx								//waveSuffix
											//is the suffix string used to specify target waves.

	variable/g datatype
	if(waveexists($"expinfo")==0)
		return ERROR_INVALID_RES_WAVE
	endif
	wave exw = $"expinfo"
	variable wln = exw[1]
	variable dstats = exw[2]
	wave/t rn = $"rn"
	variable i,n
	string wn
	variable sp,vp
	sp = -1
	n = strlen(sfx)
	for(i=0;i<n;i+=1)
		if((grepstring(sfx[i],"\d"))&&(sp==-1))
			sp = i
			break
		endif
	endfor
	if(sp==-1)
		vp = 0
	else
		vp = str2num(sfx[sp,inf])+1
	endif
	if(datatype&1)
		vp=1
	endif
	Make/o/n=6 $"stats_avg"+sfx,$"stats_stddev"+sfx,$"stats_stderr"+sfx
	wave stats_avg = $"stats_avg"+sfx
	wave stats_stddev = $"stats_stddev"+sfx
	wave stats_stderr = $"stats_stderr"+sfx

	Make/o/n=6/t stats_name

	for(i=1;i<7;i+=1)
		wn = rn[i][0] + sfx
		setscale/p x,1,1,"" $wn
		wavestats/q $wn

		stats_name[i-1] = rn[i][2]
		stats_avg[i-1] = v_avg
		stats_stddev[i-1] = v_sdev
		stats_stderr[i-1] = v_sem
	endfor
//print rn[1][0]+sfx
	setscale d,0,0,"A" $rn[1][0]+sfx
	setscale d,0,0,"C" $rn[2][0]+sfx
	for(i=3;i<7;i+=1)
		wn = rn[i][0] + sfx
		setscale d,0,0,"s" $wn
	endfor
	for(i=7;i<9;i+=1)
		wn = rn[i][0] + sfx
		setscale/p x,1,1,"" $wn
		setscale d,0,0,"A" $wn
	endfor
	wave/t rnw
	string plotstats = rnw[4]
	string windowname = replacestring(".",lowerstr(plotstats+sfx+"t"),"p")

	killwindows(windowname,"",1)
	string gsfx
	if(datatype&2)
		if(strlen(sfx))
			gsfx = "_Train_"+sfx
		else
			gsfx = "_All"
		endif
	else
		if(datatype&1)
			if(strlen(sfx)==0)
				gsfx = "_All"+"_n="+num2str(numpnts($wn))
			else
				gsfx = "_"+sfx+"s_n="+num2str(numpnts($wn))
			endif
		endif
	endif
	string wl = ""
	wl = winlist("*Statistics*",";","WIN:7")
	variable winn = itemsinlist(wl)
	if(dstats)
		for(i=0;i<winn;i+=1)
			//	print stringfromlist(i,wl)
			dowindow/hide=1 $stringfromlist(i,wl)
		endfor
	endif
	Edit stats_name,stats_avg,stats_stddev,stats_stderr as plotstats+gsfx
	ModifyTable title(stats_name)="Property",title(stats_avg)="Mean", title(stats_stddev)="S.D.",title(stats_stderr)="S.E.M."
//	print windowname
	dowindow/c $windowname
	ShowTable(windowname,vp)
	if(dstats==TRUE)
		string windowpref
		//print sfx
		windowpref = "property_"
		wl = winlist("!"+windowpref+"*",";","WIN:7")
		wl = removefromlist(winlist("*Statistics*",";","WIN:7"),wl,";",0)
		HideProcedures
		winn = itemsinlist(wl)
		for(i=0;i<winn;i+=1)
	//		print stringfromlist(i,wl)
			dowindow/hide=1 $stringfromlist(i,wl)
		endfor
		if(strlen(winlist("ProgressBar",";","WIN:64")))
			ControlInfo/w=ProgressBar prgs
			variable prov = v_value
		endif
		if(datatype&3)
			for(i=1;i<7;i+=1)
				windowname = replacestring(".",rn[i][4]+sfx+"g","p")
				killwindows(windowname,"",1)
				if(strlen(winlist("ProgressBar",";","WIN:64")))
					ValDisplay prgs,value= _NUM:round(prov+0.02*i*prov),win=ProgressBar
					TitleBox pbtx,title="Making graph...",win=ProgressBar
					DoUpdate /W=ProgressBar
				endif
				showwave(rn[i][0]+sfx,i+10)

		//		print sfx,windowname
				dowindow/c/t $windowname,rn[i][3]+gsfx
				SetAxis left 0,*
				SetAxis bottom 0,numpnts($rn[i][0]+sfx)+1
				ModifyGraph mode=1
			endfor
		endif
		if(datatype&2)
			windowname = lowerstr("eEPSC_Statistics_Train"+sfx) + "t"
//			print windowname

			killwindows(windowname,"",1)
			edit as "Property_"+gsfx[1,inf]
			dowindow/c $windowname
			for(i=1;i<7;i+=1)
				appendtotable/w=$windowname $rn[i][0]+sfx
			endfor
			ShowTable(windowname,wln+2+vp)
		endif

	endif
	return ERROR_SUCCESS
End
Override Function Correlation(ywn,xwn)
											//shows correlation between properties of calculated wave.
											//call: ShowWave
	string ywn								//yWaveName
											//is the name of wave which will be used as y-axis data.
	string xwn								//xWaveName
											//is the name of wave which will be used as x-axis data.
	string winn="",wint="",tl="",wl=""
	variable xwni,ywni,tri,i,n
	if(waveexists($"rn")==0)
		return ERROR_INVALID_RES_WAVE
	endif
	wave/t rn
	if((waveexists($xwn)==0)||(waveexists($ywn)==0))
		for(i=1;i<9;i+=1)
			wl += rn[i][2]+";"
		endfor
		prompt xwni,"Choose X-wave:",popup,wl
		prompt ywni,"Choose Y-wave:",popup,wl
		doprompt "Choose waves for correlation analysis",xwni,ywni
		if(v_flag)
			return ERROR_USER_CANCELED
		endif
		xwn = rn[xwni][0]
		ywn = rn[ywni][0]
	else
		for(xwni=1;xwni<9;xwni+=1)
			if(grepstring(xwn,"^(?i)"+rn[xwni][0]))
				break
			endif
		endfor
		for(ywni=1;ywni<9;ywni+=1)
			if(grepstring(ywn,"^(?i)"+rn[ywni][0]))
				break
			endif
		endfor

	endif

//	print xwn,ywn,xwni,ywni
	if((waveexists($xwn)==0)||(waveexists($ywn)==0))
		return ERROR_INVALID_WAVE
	endif

	wint = replacestring(" ","Correlation_"+rn[ywni][2]+"_"+rn[xwni][2],"_")
	winn = "Correlation_"+ywn+"_"+xwn+"g"
//	print winn
	if(strlen(winlist(winn,";",""))==0)
		n = itemsinlist(winlist("correlation_*",";",""))
		showwave(winn,10+n)
		dowindow/c/t $winn, wint
		appendtograph/w=$winn $ywn vs $xwn
		ModifyGraph/w=$winn mode=3,marker=8
		SetAxis left 0,1.05*wavemax($ywn)
		SetAxis bottom 0,1.05*wavemax($xwn)
	else
		dowindow/f $winn
	endif
	return ERROR_SUCCESS
End
Override Function PairPulse()
											//calculates eEPSC paired-pulse response ratios.
											//details: A dialog will be provided for user to choose
											//_value(s) from which train(s) will be calculated.
											//call: ShowTable
	if(waveexists($"expinfo")==0)
		return ERROR_INVALID_RES_WAVE
	endif
	wave exw = $"expinfo"
	variable wln = exw[1]
//	string/g postwl
	wave/t rn
	string winn,wint
	winn = rn[14][4]+"t"
	wint = rn[14][3]

	wave stimfiber = $"stimfiber"
	variable n = numpnts(stimfiber)/5
	variable i
	string chstr ="All;"
	string s
	for(i=0;i<n;i+=1)
		s=num2str(i)+": "+num2str(stimfiber[i][1])+"Hz * "+num2str(stimfiber[i][2])+";"
		chstr+=s
	endfor

	variable ch = 1
	prompt ch,"Choose Wave:",popup,chstr
	doprompt "Pair Pulse Option",ch
	if(v_flag)
		return ERROR_USER_CANCELED
	else
		string epscpref
		wave/t rn
		epscpref = rn[10][0]

		ch-=2
		chstr = epscpref

		string wnf,wns

		if(ch!=ALL)
			wnf = chstr+num2str(ch)+"_"+"0"
			wns = chstr+num2str(ch)+"_"+"1"

//			print "Pair Pulse Ratio = "num2str(wavemin($wns,0,inf) /wavemin($wnf,0,inf))
		else
			make/n=(wln)/o ppr
			for(i=0;i<wln;i+=1)

				wnf = chstr+num2str(i)+"_"+"0"
				wns = chstr+num2str(i)+"_"+"1"
				ppr[i]= wavemin($wns,0,inf) /wavemin($wnf,0,inf)
			endfor
			string windowlist = winlist(winn,";","Win:7")
			variable windowln = ItemsInList(windowlist)
			if(windowln==0)
				edit ppr as wint

				dowindow/c $winn

			endif
			ShowTable(winn,wln+2)
		endif
	endif
	return ERROR_SUCCESS
End
Override Function RRPSize(twi,rmode)
											//calculates readily releasable pool sizes.
											//_Both absolute and normalized sizes and slopes are estimated.
											//details: A dialog will be provided for user to choose
											//_value(s) from which train(s) to be calculated by which method(s).
											//call: ShowWave, ShowTable, KillWave, KillWindows
	variable twi							//trainIndex
											//is used to specify the target train to which the calculation will be
											//_applied. The value could be:
											//  -2    User select.
											//  -1    All trains.
											//  i    The train indexed i.
	variable rmode							//dataMode
											//is used to specify which method(s) will be used to calculate the
											//_RRP size and mobilization rate.
											//  -2    User select.
											//  -1    Both methods will be used.
											//  1    Calculate RRP by accumulated amplitude of eEPSCs.
											//  2    Calculate RRP by accumulated total charge of eEPSCs.
	variable/g datatype
	if((datatype&2)==0)
		print "This Function is for eEPSC Data Only."
		return ERROR_NO_TARGET_WAVE
	endif
	wave/t rn = $"rn"
	string winnr = rn[13][4]+"t"			//rrp table window name
	string wintr = rn[13][3]				//rrp table window title
	variable i,n
	if(waveexists($"expinfo")==0)
		return ERROR_INVALID_RES_WAVE
	endif
	wave exw = $"expinfo"
	variable wln = exw[1]
	variable dstats = exw[2]
	wave stimfiber = $"stimfiber"

	wave/t orgname = $"orgname"
	if((twi==SELECT)||(rmode==SELECT))				//prompt dialog if SELECT
		if(rmode==ALL)
			rmode+=2
		else
			rmode+=1
		endif
		string chstr ="All;"
		string s
		for(i=0;i<wln;i+=1)
			s=num2str(i)+": "+num2str(stimfiber[i][1])+"Hz * "+num2str(stimfiber[i][2])+";"
			chstr+=s
		endfor

		variable ch = twi+2
		prompt ch,"Choose wave:",popup,chstr
		prompt rmode,"Choose mode",popup,"All;Amplitude;Total Charge"
		doprompt "RRP size option",ch,rmode
//		doprompt "Average Option",ch
		if(v_flag)
			return ERROR_USER_CANCELED
		else
			if(rmode==1)
				rmode -= 2
			else
				rmode -= 1
			endif

			ch-=2

			RRPSize(ch,rmode)
			return ERROR_SUCCESS
		endif
	endif
	if((rmode!=1)&&(rmode!=2)&&(rmode!=ALL))	//Use amplitude accumulation as default method
		rmode = 1
	endif

	string windowlist
	variable windowln

	if(twi==ALL)								//loop for all trains
		make/o/n = (wln,6) rrp = 0			//is a plot window which contains the rrp
												//_size and the slope of all trains.
												//rrp is a (n,6) two-dimensional wave (matrix),
												//_where n stands for the number of the eEPSC trains.
												//_The columns are as:
												//  column 0:    Absolute size calculated by amplitude.
												//  column 1:    Absolute slope calculated by amplitude.
												//  column 2:    Normalized slope calculated by amplitude.
												//  column 3:    Absolute size calculated by total charge.
												//  column 4:    Absolute slope calculated by total charge.
												//  column 5:    Normalized slope calculated by total charge.
//		rrps = 0
//		rrpsl = 0
		for(i=0;i<wln;i+=1)
			RRPSize(i,rmode)
		endfor


		if(dstats==TRUE)							//display table if dstats is true
			windowlist = winlist(winnr,";","Win:7")

			if(strlen(windowlist)==0)
			//	print "loop rrp"
				edit rrp as wintr

				dowindow/c $winnr
			endif
			ShowTable(winnr,wln+1)
		endif
		return ERROR_SUCCESS
	endif
	if(rmode==ALL)									//use both two method
		RRPSize(twi,1)
		RRPSize(twi,2)
		return ERROR_SUCCESS
	endif
	if(!waveexists($"rrp"))						//make plot wave if not exist
		make/o/n = (wln,6) rrp
	endif

	wave rrp = $"rrp"
//	wave rrpsl = $"rrpsl"
	string wnac										//absolute accumulate wave name
	string wnar										//absolute fit wave name
	string winna									//absolute window name
	string winta									//absolute window title
	string wnnc										//normalized accumulate wave name
	string wnnr										//normalized fit wave name
	string winnn									//normalized window name
	string wintn									//normalized window title

	wnar = rn[13+2*rmode][0] + num2str(twi)					//e.g. "ara0" / "arc0"
	wnac = "ac" + wnar[2] + num2str(twi)						//e.g. "aca0" / "acc0"
	winna = rn[13+2*rmode][4] + num2str(twi) + "g"
	winta = rn[13+2*rmode][3] + "_Train_"+num2str(twi)
	wnnr = rn[13+2*rmode+1][0] + num2str(twi)				//e.g. "nra0" / "nrc0"
	wnnc = "nc" + wnnr[2] + num2str(twi)						//e.g. "nca0" / "ncc0"
	winnn = rn[13+2*rmode+1][4] + num2str(twi) + "g"
	wintn = rn[13+2*rmode+1][3] + "_Train_"+num2str(twi)
//	print  wnac,wnar,winna,winta,wnnc,wnnr,winnn,wintn
//	abort
	wave src = $rn[rmode][0]+num2str(twi)						//source wave depending on method


	windowlist = winlist(winna,";","Win:7")
	windowln = ItemsInList(windowlist)
	if(windowln!=0)											//kill abs window if exist
		killwindows(winna,"",1)
	endif
	windowlist = winlist(winnn,";","Win:7")
	windowln = ItemsInList(windowlist)
	if(windowln!=0)											//kill norm window if exist
		killwindows(winnn,"",1)
	endif
	if(waveexists(src))										//do only source wave exist
		integrate src /d=$wnac									//make accumulative wave
		wave ac = $wnac
		ac = abs(ac)

		duplicate/o ac $wnnc									//make norm copy (not normalized yet)
		wave nc = $wnnc
		n = numpnts(ac)

		variable V_FitError

		CurveFit/NTHR=0/Q/N/W=2 line, ac(n/2,n)/D		//fit later half of abs accumulative wave
		//	abort
		if(v_fiterror!=0)
			print wnnc +" Fit Error. ErrorCode:",v_fiterror
		endif
		wave w_coef = $"W_coef"
		//abort
		rrp[twi][(rmode-1)*3] = w_coef[0]					//output size, slopes
		rrp[twi][(rmode-1)*3+1] = w_coef[1]
		rrp[twi][(rmode-1)*3+2] = w_coef[1] / w_coef[0]
		if(V_FitError)
			print wnnc+" FitError:",V_FitError
		endif
	//	print wi
		if(waveexists($"fit_"+wnnc))							//kill auto made fit wave
			killwave("fit_"+wnnc,TRUE,TRUE)
		endif
		nc /= rrp[twi][(rmode-1)*3]							//normalize accumulative wave
		make/o/n = (n+1) $wnar									//make fit abs accumulative wave

		wave ar = $wnar
		for(i=0;i<n+1;i+=1)									//make abs full-length line wave
			ar[i]= rrp[twi][(rmode-1)*3]+i*rrp[twi][(rmode-1)*3+1]
		endfor
		duplicate/o ar $wnnr									//make norm full-length line wave
		wave nr = $wnnr
		nr /= rrp[twi][(rmode-1)*3]

		if(dstats == 1)
//print winna,winnn
			showwave(wnac,10+twi*4+(rmode-1)*2)				//display abs waves
			ModifyGraph mode($wnac)=3,marker($wnac)=8
			appendtograph ar
			dowindow/c/t $winna,winta
			SetAxis bottom 0,*
			SetAxis left 0,*

			showwave(wnnc,10+twi*4+(rmode-1)*2+1)				//display norm waves
			ModifyGraph mode($wnnc)=3,marker($wnnc)=8
			appendtograph nr
			dowindow/c/t $winnn,wintn
			SetAxis bottom 0,*
			SetAxis left 0,*

			windowlist = winlist(winnr,";","Win:7")
			if(strlen(windowlist)==0)
				edit rrp as wintr
				dowindow/c $winnr
			endif
			ShowTable(winnr,wln+1)
		endif
	endif
	return ERROR_SUCCESS
End
Override Function SteadyPhase(twi)
											//calculates the properties of eEPSC waves from steady phase in trains.
											//call: Average, StatseEPSC
	variable twi							//trainIndex
											//is used to specify the target train to which the calculation will be
											//_applied. The value could be:
											//  -2    User select.
											//  -1    All trains.
											//  i    The train indexed i.
	if(waveexists($"expinfo")==0)
		return ERROR_INVALID_RES_WAVE
	endif
	wave exw = $"expinfo"
	variable wln = exw[1]
//	string/g postwl

	wave stimfiber = $"stimfiber"
	variable n = numpnts(stimfiber)/5
	variable i
	string chstr =""
	string s
	for(i=0;i<n;i+=1)
		s=num2str(i)+": "+num2str(stimfiber[i][1])+"Hz * "+num2str(stimfiber[i][2])+";"
		chstr+=s
	endfor

	variable swi,ewi
	if(twi>ALL)
		swi = min(30,0.6*stimfiber[twi][2])
		ewi = min(49,stimfiber[twi][2]-1)
	else
		swi = 30
		ewi = 49
	endif
//	print swi,ewi
	variable ch = twi
	if((twi<0)||(twi>n-1))
		ch += 1
		prompt swi,"Start Wave:"
		prompt ewi,"End Wave:"
		prompt ch,"Choose Wave:",popup,chstr
		doprompt "Steady Phase Option",ch,swi,ewi
		//	doprompt "Average Option",ch
		if(v_flag==1)
			return ERROR_USER_CANCELED
		endif
		ch-=1
	endif
	string epscpref
	wave/t rn
	epscpref = rn[10][0]


	chstr = epscpref

	Average(chstr+num2str(ch)+"_",swi,ewi)
	string wn = "avg_"+chstr+num2str(ch)
	string wnsp = chstr+num2str(ch)+"_"+num2str(swi)+"_"+num2str(ewi)
	if(waveexists($wn))
		duplicate/o $wn $"savg"+num2str(ch)

		StatseEPSC(wnsp)
	endif
	return ERROR_SUCCESS
End
Override Function ExportStats(twi,sfn,apd)
											//exports eEPSC waves, properties, plasticity results, etc.
											//_of target train(s) or cell.
											//call: BatchKill, KillWave, GetRecentPath
	variable twi							//trainIndex
											//is a used to specify the target train(s), whose relevant
											//_waves will be exported. The value could be:
											//  -2    User select.
											//  -1    Waves calculated from all trains.
											//  i    Waves caclulated from the train indexed i.
	string sfn								//saveFileName
											//is the file name to which the waves will be exported to.
	variable apd							//appendFlag
											//is a flag for program to decide the file output behaviour. The
											//_values of apd should be:
											//  FALSE    Create a new file named sfn for export.
											//  TRUE    Append the waves to a file which already exists.

	variable/g datatype
	if((datatype&2)==0)
		print "This Function is for eEPSC Data Only."
		return ERROR_NO_TARGET_WAVE
	endif
	wave/t rn = $"rn"

	string epscpref
	wave/t rn
	epscpref = rn[10][0]

	variable i
	if(waveexists($"expinfo")==0)
		return ERROR_INVALID_RES_WAVE
	endif
	wave exw = $"expinfo"
	variable wln = exw[1]

	wave stimfiber = $"stimfiber"
	variable n = numpnts(stimfiber)/5
	wave/t rchmap = $"rchmap"
	if((twi==SELECT)||(strlen(sfn)==0))
		string chstr ="All;"
		string s
		for(i=0;i<n;i+=1)
			s=num2str(i)+": "+num2str(stimfiber[i][1])+"Hz * "+num2str(stimfiber[i][2])+";"
			chstr+=s
		endfor

		variable ch = twi + 2
		string/g fn
		if(strlen(sfn)==0)
			n = strsearch(fn,"\\",inf,1)
			sfn = fn[n+1,strlen(fn)-5] + "_EXPORT"

		endif
		prompt ch,"Choose Wave:",popup,chstr
		prompt sfn,"Output File Name:"
		prompt apd,"File Output Behaviour:",popup,"Create new file;Append to file already exists"
		doprompt "Export Option",ch,sfn,apd
		if(v_flag)
			return ERROR_USER_CANCELED
		else
			ch-=2
			apd -= 1

			if(ch!=ALL)
				sfn += "_Train_"+num2str(ch)
			endif

			ExportStats(ch,sfn,apd)

			return ERROR_SUCCESS
		endif

	endif

	if(twi==ALL)
		if(apd)
			for(i=0;i<wln;i+=1)
				ExportStats(i,sfn,apd)
			endfor
		else
			ExportStats(i,sfn,apd)
			for(i=1;i<wln;i+=1)
				ExportStats(i,sfn,1)
			endfor
		endif
		batchkill("f"+rn[10][0],3)


		return ERROR_SUCCESS
	endif

	string swl = ""								//wavelist for save
	string kswl = ""								//wavelist for kill renamed wave
	String/g fn										//data file name
	string wn,dn,wno,fstr


	variable j = 0


	wno = replacestring("_",stringfromlist(twi,rchmap[0][6],";"),"",1,3)
	fstr = num2str(stimfiber[twi][1])+"_"

	for(j=1;j<9;j+=1)								//8 properties
		wn = rn[j][0]+num2str(twi)
		dn = rn[j][0]+"_" + fstr + wno
		if(waveexists($wn))
			duplicate/o $wn,$dn
			swl += dn + ";"
			kswl +=dn +","
		endif
	endfor

	for(j=1;j<3;j+=1)								//normalized amp,tc
		wn = rn[j][0]+num2str(twi)
		dn = "n"+rn[j][0]+"_" + fstr + wno
		if(waveexists($wn))
			duplicate/o $wn,$dn
			wave nor = $dn
			n = nor[0]
			nor/=n
			swl += dn + ";"
			kswl +=dn +","
		endif
	endfor

	wn = "ac"+num2str(twi)							//accumulative amp
	dn = "ac_" + fstr + wno
	if(waveexists($wn))
		duplicate/o $wn,$dn
		swl += dn + ";"
		kswl +=dn +","
	endif
	wn = "n"+wn										//normalized acc amp
	dn = "n_" + dn
	if(waveexists($wn))
		duplicate/o $wn,$dn
		swl += dn + ";"
		kswl +=dn +","
	endif

//	wn = rn[2][0]+num2str(twi)						//accumulative tc
//	dn = "actc_"+ fstr + wno
//	if(waveexists($wn))
//		integrate $wn /d=$dn
//		swl += dn + ";"
//		kswl +=dn +","
//	endif

	wn = "a"+rn[13][0]+num2str(twi)				//absolute rrp
	dn = "a"+rn[13][0]+"_" + fstr + wno
	if(waveexists($wn))
		duplicate/o $wn,$dn
		swl += dn + ";"
		kswl +=dn +","
	endif
	wn = "n"+rn[13][0]+num2str(twi)				//normalized rrp
	dn = "n"+rn[13][0]+"_" + fstr + wno
	if(waveexists($wn))
		duplicate/o $wn,$dn
		swl += dn + ";"
		kswl +=dn +","
	endif

	wn = "savg"+num2str(twi)
	dn = "savg_" + fstr + wno
	if(waveexists($wn))
		duplicate/o $wn,$dn
		swl += dn + ";"
		kswl +=dn +","
	endif
	
	for(j=15;j<19;j+=1)								//ara, nra, arc,nrc
		wn = rn[j][0]+num2str(twi)
		dn = rn[j][0]+"_" + fstr + wno
		if(waveexists($wn))
			duplicate/o $wn,$dn
			swl += dn + ";"
			kswl +=dn +","
		endif
	endfor
	
	wn = "aca"+num2str(twi)
	dn = "aca_" + fstr + wno
	if(waveexists($wn))
		duplicate/o $wn,$dn
		swl += dn + ";"
		kswl +=dn +","
	endif

	wn = "nca"+num2str(twi)
	dn = "nca_" + fstr + wno
	if(waveexists($wn))
		duplicate/o $wn,$dn
		swl += dn + ";"
		kswl +=dn +","
	endif
	wn = "acc"+num2str(twi)
	dn = "acc_" + fstr + wno
	if(waveexists($wn))
		duplicate/o $wn,$dn
		swl += dn + ";"
		kswl +=dn +","
	endif
	wn = "ncc"+num2str(twi)
	dn = "ncc_" + fstr + wno
	if(waveexists($wn))
		duplicate/o $wn,$dn
		swl += dn + ";"
		kswl +=dn +","
	endif

	n = strsearch(fn,"\\",inf,1)


	wno = replacestring("-",fn[n+1,strlen(fn)-5],"_",1)
	wno = replacestring("_",wno,"",1,3)
	wn = rn[10][0]+"0_0"
	dn = "f"+rn[10][0]+"_" + wno
	if((waveexists($wn))&&(waveexists($dn)==0))

		duplicate/o $wn,$dn
		swl += dn + ";"
		if(strsearch(sfn,"Train",0)>-1)			//Avoid redundant add fepsc, nfepsc
			kswl +=dn +","
		endif
		dn = "n" + dn
		duplicate/o $wn $dn
		wave nor = $dn
		n = wavemin(nor,-5e-4,1e-3)
		nor /= abs(n)
		swl += dn + ";"
		if(strsearch(sfn,"Train",0)>-1)
			kswl +=dn +","
		endif
	endif

	string path = GetRecentPath(ALL,TRUE)

	//	variable m = 0
	//	variable ka = 0
	//
	//	if(exists("app")==0)
	//		ka = 1
	//	endif
	//	variable/g app									//append,1==yes,0==no
	//
	//	print swl
	//	do
	//		string bswl=""
	//		string bkswl=""
	//		if(strlen(swl)>300)
	//			n = strsearch(swl,";",300,1)
	//			bswl = swl[n+1,inf]
	//			swl = swl[0,n]
	//			bkswl = kswl[n+1,inf]
	//			kswl = kswl[0,n]
	//		endif

	if(apd==FALSE)
		save/t/b swl as path+sfn+".itx"
		if(strlen(kswl)!=0)
			killwave(kswl,TRUE,TRUE)
		endif
	else
		save/t/a/b swl as path+sfn+".itx"
		if(strlen(kswl)!=0)
			killwave(kswl,TRUE,TRUE)
		endif
	endif
//		if(strlen(bswl)==0)
//			break
//		else
//			swl = bswl
//			kswl = bkswl
//			m+=1
//			apd = 1
//		endif
//	while(1)
//	if(ka==1)
//		killvariables $"app"
//	endif
	return ERROR_SUCCESS
End
Override Function Import(imode,lfn)
											//imports waves for inter-experiment analysis.

											//call: Initial, LoadITX, SetDatatype, CheckSF, ShowAverage, Average, MakeMnReg, CleanUp, KillSingle
	variable imode							//importType
											//is used to specify the type of waves which are to be imported.
											//_The values are defined as:
											//  -2    User select.
											//  0    Average.
											//  1    Amplitude.
											//  2    Total charge.
											//  3    Half width.
											//  4    Rise time.
											//  5    Decay tau1.
											//  6    Decay tau2.
											//  7    Decay a1.
											//  8    Decay a2.
											//  9    mEPSC.
											//  10    eEPSC.
											//  11    Ica.
											//  12    Double patch data.
											//  13    Readily releasable pool.
											//  14    Paired-pulse ratio.
	string lfn								//dataFileName
											//is used to specify the file to open. If it is a partial
											//_name, the file in the same folder with the saved experiment
											//_file, recently opened ITX, or the user procedure will be tried.

	variable ch = 0
	variable i,n
//	variable/g isinit
	string wl

	wl = wavelist("*",";","")
	n = itemsinlist(wl)
	string wn
	variable/g expsign
//	if(isinit!=1)//||(n-waveexists($"ExpFD")-waveexists($"rn")<=0))


	if(imode==SELECT)
		wave/t rn = $"rn"
		string el=""
		n = dimsize(rn,0)
		for(i=0;i<n;i+=1)
			el+= rn[i][2]+";"
		endfor
		prompt ch,"Choose Wave to Export:",popup,el
		doprompt "Set Import Parameter:",ch
		if(v_flag)
			return ERROR_SUCCESS
		else
			ch-=1
			Import(ch,lfn)
			return ERROR_SUCCESS
		endif
	endif
	if((imode<0)||(imode>14))
		print "Wrong index to import."
		return ERROR_INDEX_OUT_OF_RANGE
	endif
	CleanUp()
	Initial()
	LoadITX(2,lfn)
	CheckSF()
//	i=strsearch(lfn,"\\",0)
//
//	if(i==-1)
//		string pl = pathlist("!Igor",";","")
//		string path
//		if(strsearch(pl,"home",0)!=-1)
//			path = "home"
//		else
//			path = stringfromlist(0,pl,";")
//		endif
//		pathinfo $path
//		lfn = s_path + lfn
//		lfn = replacestring(":",lfn,"\\")
//		lfn = replacestring("\\",lfn,":\\",0,1)
//	endif
//
//	print "Start Loading..."
//
//	variable ref
//	open/r/z=1 ref as lfn							//check existence of data file
//
//	if(ref!=0)
//		close ref
//	endif
//	if ((strlen(lfn)==0)||(v_flag!=0))
//
//		loadwave/q/o/t
//								//load igor text file
//		if(v_flag==0)
//			print "LoadITX Canceled"
//			isinit = 0
//			return //-1
//		endif
//
//		string fn = s_filename		//get itx file name
//
//		s_path = replacestring(":",s_path,"\\")
//		s_path = replacestring("\\",s_path,":\\",1,1)
//		fn = s_path+fn
//	else
//		loadwave/q/o/t lfn
//		fn = lfn
//	endif
//	print "Data Import from File \""+fn+"\" Complete."

	SetDataType()
	string pref
	switch(imode)
		case 0:
//			wn = stringfromlist(0,wavelist("avg_m*",";",""),";")
//			SetSign(expsign)
//			wl = wavelist("avg_*",";","")
//			for(i=0;i<itemsinlist(wl);i+=1)
//				wn = stringfromlist(i,wl,";")
//				wave w = $wn
//				n = str2num(wn[strsearch(wn,"_",inf,1)+1,inf])
//				w *= n
//			endfor
//			if(datatype&7)
//				Average("Avg_mEPSC",0,inf)
//				Average("Avg_Ca",0,inf)
//			else
				Average("Avg_",0,inf)
//			endif
			break
		case 1:
		case 2:
		case 3:
		case 4:
		case 5:
		case 6:
		case 7:
		case 8:
		case 9:
		case 10:
		case 11:
		case 12:
			variable/g datatype
			if(datatype&1)
				MakeMnReg()
				if(datatype&4)
					KillSingle()
					MakeMnReg()
				endif
				ShowAverage()
			endif
			break
		case 13:
		case 14:
			break
		default:
			print "Wrong index to import."
			return ERROR_INDEX_OUT_OF_RANGE
	endswitch
	return ERROR_SUCCESS
End
Override Function Export(emode,wi,sfn,sfx,apd)
											//exports data for inter-experiment analysis.

											//call: GetWaveList, KillWave, GetRecentPath
	variable emode							//exportType
											//is used to specify the type of waves which are to be exported.
											//The available values could be:
											//  -2    User select.
											//  0    Average.
											//  1    Amplitude.
											//  2    Total charge.
											//  3    Half width.
											//  4    Rise time.
											//  5    Decay tau1.
											//  6    Decay tau2.
											//  7    Decay a1.
											//  8    Decay a2.
											//  9    mEPSC.
											//  10    eEPSC.
											//  11    Ica.
											//  12    Double patch data.
											//  13    Readily releasable pool.
											//  14    Paired-pulse ratio.
	variable wi								//waveIndex
											//is used to select the exact waves to export according to their
											//_indexes in the list. When the value is set as -1, all waves matches
											//_the emode will be selected.
	string sfn								//saveFileName
											//is the file name to which the waves will be exported to.
	string sfx								//waveSuffix
											//is a suffix which may be appended to the orginal names for avoiding
											//_naming conflicts in the target file.
	variable apd							//appendFlag
											//is a flag for program to decide the file output behaviour. The
											//_values of apd should be:
											//  FALSE    Create a new file named sfn for export.
											//  TRUE    Append the waves to a file which already exists.

	string pref,savename

	variable/g datatype
	variable ch = wi

	variable i,n

	if(emode==SELECT)
		ch+=1
		wave/t rn = $"rn"
		string el=""
		n = numpnts(rn)/dimsize(rn,1)
		for(i=0;i<n;i+=1)
			el+= rn[i][2]+";"
		endfor
		prompt ch,"Choose Wave to Export:",popup,el
		doprompt "Set Export Parameter:",ch
		if(v_flag)
			return ERROR_USER_CANCELED
		else
			ch-=1
			Export(ch,1,sfn,sfx,apd)
			return ERROR_SUCCESS
		endif
	endif




	string minilpref,calpref
	wave/t rn
	minilpref = rn[9][1]

	calpref = rn[11][1]

	wave/t rn = $"rn"
	if((emode>-1)&&(dimsize(rn,0)))			//standard series
		pref = rn[emode][1]
	else
		string expstr = strvarordefault("root:expstr","")
		if(strlen(expstr)==0)
			prompt pref,"Wave Name:"
			doprompt "Input name pattern",pref
			if(v_flag)
				return ERROR_USER_CANCELED
			endif
		else
			pref = expstr
		endif
	endif
	savename = replacestring(" ",rn[emode][2],"_")+"_Plot"

	string wl = GetWaveList(pref+"[TEXT:0]",1,TRUE)				//only numeric waves are considered.
	if (cmpstr(pref,"dbp*")==0)
		wl = GetWaveList(minilpref+";"+calpref+"[TEXT:0]",1,TRUE)
	endif
//	print itemsinlist(GetWaveList(minilpref+";"+calpref+"[TEXT:0]",1,TRUE))
//	print itemsinlist(GetWaveList(minilpref+";"+calpref+"[TEXT:0];",1,TRUE))
//	print itemsinlist(GetWaveList(calpref+"[TEXT:0]",1,TRUE))
//print pref,calpref

	variable wln = ItemsInList(wl)
	if(wln!=0)								//do only list not empty

		sfn = replacestring("*",sfn,"")
		sfn = replacestring(" ",sfn,"")

		if((strlen(sfn)==0))//||(apd==0))
			sfn = savename
			ch = wi+2
			string pwl = "All;"+wl
			apd += 1

			prompt ch,"Existed Wave:",popup,pwl
			prompt apd,"File Output Option:",popup,"Create a new file;Append to a file already exists."
			prompt sfx,"Wave Name Suffix:"
			prompt sfn,"Export File Name:"
			doprompt "Choose wave to export:",ch,apd,sfx,sfn
			if(v_flag==1)
				return ERROR_USER_CANCELED
			endif
			apd-=1
			ch-=2
		endif

		if(strlen(sfn)==0)
			sfn = savename
		endif

		if(strlen(sfx)!=0)
			sfx = "_"+sfx
		endif
		string own,ewn,swl = "",kswl = ""
		if(cmpstr(parsefilepath(4,sfn,":",0,0),"itx"))
			sfn = sfn+".itx"
		endif
		string path = GetRecentPath(ALL,TRUE)
		variable overwt = numvarordefault("root:overwt",0)
		if(ch==ALL)

			for(i=0;i<itemsinlist(wl);i+=1)
				own =StringFromList(i,wl,";")

				ewn = own + sfx
				//							print own,ewn
				//							print strlen(own),strlen(ewn)
				if(cmpstr(own,ewn)!=0)
					duplicate/o $own, $ewn
					kswl +=ewn+","
				endif
				swl +=ewn+";"
			endfor
			//	variable m = 0
			//	string path = stringfromlist(0,pathlist("!Igor",";",""),";")

			//			print apd
			//			do
			//
			//
			//				string bswl=""
			//				string bkswl=""
			//				if(strlen(swl)>300)
			//					n = strsearch(swl,";",300,1)
			//					bswl = swl[n+1,inf]
			//					swl = swl[0,n]
			//					bkswl = kswl[n+1,inf]
			//					kswl = kswl[0,n]
			//				endif

			if(apd==FALSE)
				save/t/b swl as path + sfn
				if(strlen(kswl)!=0)
					killwave(kswl,TRUE,TRUE)
				endif
			else
				if(overwt)
					save/t/o/a/b swl as path + sfn
				else
					save/t/a/b swl as path + sfn
				endif
				if(strlen(kswl)!=0)
					killwave(kswl,TRUE,TRUE)
				endif
			endif
//				if(strlen(bswl)==0)
//					break
//				else
//					swl = bswl
//					kswl = bkswl
//					m+=1
//				endif
//
//			while(1)

		else
			own =StringFromList(ch,wl,";")
			ewn = own + sfx
			if(cmpstr(own,ewn)!=0)
				duplicate/o $own, $ewn
			endif
			if(apd==FALSE)
				save/t $ewn as path+sfn
			else
				if(overwt)
					save/t/a/o $ewn as path + sfn
				else
					save/t/a $ewn as path + sfn
				endif
			endif
			if(cmpstr(own,ewn)!=0)
				killwave(ewn,TRUE,TRUE)
			endif
		endif

//		s_path = replacestring(":",s_path,"\\")
//		s_path = replacestring("\\",s_path,":\\",1,1)
		print "Data Export to File \""+path+sfn+"\" Complete."
	else
	//		if(datatype==1)
	//			if(waveexists($"mntime"))
	//
	//				if(numpnts(mntime)!=0)
	//					print "Do data analysis first!"
		return ERROR_NO_TARGET_WAVE
	//				endif
	//			endif
	//		else
	//			if(datatype==2)
	//			//
	//			endif
	//		endif
	//
	endif
	return ERROR_SUCCESS
End
Override Function ConcatWave(matchstr,matchloc,nprm,ow,cwn)
											//concatentes waves by name patterns.
											//seealso: Concatenate, stringmatch
											//call: GetWaveList, KillWave
											//seealso: Concatenate
	string matchstr						//matchStr
											//is used for selecting target waves to concatenate.
	variable matchloc						//matchLoc
											//is the option for matching original names. The values are:
											//  0    Waves having names exactly matching to matchstr.
											//  1    Waves having names start with matchstr.
											//  2    Waves having names end with matchstr.
											//  3    Waves having names with matchstr in middle.
	variable nprm							//noPromotion
											//is the option used for deciding whether the output waves
											//_should be promoted to be multi-dimensional or not. Similar to
											//_/NP flag of the built-in concatenate operation. The values
											//_are:
											//  FALSE    Promote the output wave and make it a matrix.
											//  TRUE    Don't promote output wave to multidimension.
	variable ow								//overWriteOption
											//is used to specify function behaviour when the output wave
											//_already exists.
											//  FALSE    Don't overwrite cwn and use other valid name.
											//  TRUE    Overwrite cwn if it exists.
	string cwn								//destWaveName
											//is the name of concatenated wave for output.
	matchstr = replacestring(",",matchstr,";")

	if(strlen(matchstr)==0)
		ow += 1
		nprm += 1
		matchloc += 1
		prompt matchstr,"Wave Name Match String:"
		prompt matchloc,"String Location:",popup,"Exact Match;Starts With;Ends With;In Middle"
		prompt nprm,"Promotion:",popup,"Do Promotion;Don't Promote Output Wave"
		prompt ow,"OverWrite Option:",popup,"Don't OverWrite, Use Different Name;Over Write the Wave if Already Exists"
		prompt cwn,"Output Wave Name:"
		doprompt "Concatentate Waves",matchstr,matchloc,ow,nprm,cwn
		if(v_flag==1)
			return ERROR_USER_CANCELED
		endif
		ow -= 1
		nprm -= 1
		matchloc -= 1
	endif
//	print matchstr
	matchstr = replacestring(",",matchstr,";")

	if(strlen(cwn)==0)
		cwn = "concat_" + replacestring("*",stringfromlist(0,matchstr),"")
	endif
	switch (matchloc)
		case 1:
			matchstr = matchstr + "*"
			break
		case 2:
			matchstr = "*" + matchstr
			break
		case 3:
			matchstr = "*" + matchstr + "*"
			break
		default:
//			matchstr = "*" + matchstr + "*"
	endswitch
	string wl = ""
	variable i,n

	if(cmpstr(GetRTStackInfo(2),"StatsmEPSC"))
	//	print "xx"
		wl = GetWaveList(matchstr,TRUE,TRUE)
	else
//		print "oo",matchstr
		wl = matchstr
		concatenate/o/np=(abs(1-nprm)) wl,$cwn
		return ERROR_QUICK_MODE
	endif
//	n = itemsinlist(matchstr)
//	for(i=0;i<n;i+=1)
//		wl += wavelist(stringfromlist(i,matchstr),";","")
//	endfor
//	print wl
	if(waveexists($cwn))
		if(ow)
			killwave(cwn,TRUE,FALSE)
		else
//			for(i=0;;i+=1)
//				if(waveexists($cwn + num2str(i))==0)
//					cwn = cwn + num2str(i)
//					break
//				endif
//			endfor
		endif
	endif
	string rl = wl
	wl = ""
	string wn
	n = itemsinlist(rl)
	for(i=0;i<n;i+=1)
		wn = stringfromlist(i,rl)
		if(waveexists($wn))
			wl = wl + wn + ";"
		endif
	endfor
	if(strlen(wl)!=0)

		if(ow==1)
			concatenate/o/np=(abs(1-nprm)) wl,$cwn
		else
			concatenate/np=(abs(1-nprm)) wl,$cwn
		endif
	endif
	return ERROR_SUCCESS
End
Override Function RenameWave(matchstr,newstr,matchloc,newloc,ow)
											//renames waves by name patterns.
											//call: GetWaveList, KillWave
	string matchstr						//matchStr
											//is used for selecting the waves according to the patterns.
	string newstr							//newStr
											//is used to modify the original wave name.
	variable matchloc						//matchLoc
											//is the option for matching original names. The values are:
											//  0    Waves having names exactly matching to matchstr.
											//  1    Waves having names start with matchstr.
											//  2    Waves having names end with matchstr.
											//  3    Waves having names with matchstr in middle.
	variable newloc						//newLoc
											//is the option for making modified names. The values are:
											//  1    Add newstr before original waves.
											//  2    Append newstr after original waves.
											//  3    Replace matchstr with newstr in original waves.
	variable ow								//overWriteOption
											//is used to determine whether the overwriting should be performed
											//_if the waves with modified names are already existing. Set the
											//_argument as 1 to overwrite, or 0 to use newstr+"num" as
											//_the output name.
	if(strlen(newstr)==0)
		ow += 1
		matchloc+=1
		prompt matchstr,"Wave Name Match String:"
		prompt matchloc,"Match String Location:",popup,"Exact Match;Starts With;Ends With;In Middle"
		prompt newstr,"New Wave Name String:"
		prompt newloc,"New String Location:",popup,"Add Before;Append;Replace"
		prompt ow,"OverWrite:",popup,"Don't OverWrite, Use Different Name;Over Write the Wave if Already Exists"

		doprompt "Rename Waves",matchstr,matchloc,newstr,newloc,ow

		if(v_flag==1)
			return ERROR_USER_CANCELED
		endif
		ow-=1
		matchloc-=1
	endif

	if(newloc==0)
		newloc = 1
	endif


	if((cmpstr(matchstr,"")==0)&&(newloc==3))
		return ERROR_NOT_ALLOWED
	endif


	switch (matchloc)
		case 1:
			matchstr = matchstr + "*"
			break
		case 2:
			matchstr = "*" + matchstr
			break
		case 3:
			matchstr = "*" + matchstr + "*"
			break
		default:
	endswitch

	string wl = getwavelist(matchstr,1,TRUE)
	variable wln,i
	string wn,dwn
	wln = itemsinlist(wl)
	if(wln==0)
		return ERROR_NO_TARGET_WAVE
	endif
	matchstr = replacestring("*",matchstr,"")
	for(i=0;i<wln;i+=1)
		wn = stringfromlist(i,wl,";")
		switch (newloc)
		case 1:
			dwn = newstr + wn
			break
		case 2:
			dwn = wn + newstr
			break
		case 3:

			dwn = replacestring(matchstr,wn,newstr)

			break
		default:
		endswitch
		if(waveexists($dwn))
			if(ow)
				killwave(dwn,TRUE,FALSE)
			else
				variable j
				for(i=0;;j+=1)
					if(waveexists($dwn + num2str(j))==0)
						dwn = dwn + num2str(j)
						break
					endif
				endfor
			endif
		endif

		rename $wn $dwn
	endfor
	return ERROR_SUCCESS
End
Override Function BatchKill(matchstr,matchloc)
											//kills waves by name patterns.
											//details: After the operation has been successful executed, the
											//_emptied graphs, tables, and layouts will be closed.

											//call: GetWaveList, KillWave
	string matchstr						//matchStr
											//is used for selecting a group of waves to kill.
	variable matchloc						//matchLoc
											//is the option for matching original names. The values are:
											//  0    Waves having names exactly matching to matchstr.
											//  1    Waves having names start with matchstr.
											//  2    Waves having names end with matchstr.
											//  3    Waves having names with matchstr in middle.
	if(strlen(matchstr)==0)
		matchloc += 1
		prompt matchstr,"Wave Name Match String:"
		prompt matchloc,"Match String Location:",popup,"Exact Match;Starts With;Ends With;In Middle"

		doprompt "Kill Waves",matchstr,matchloc
		if(v_flag==1)
			return ERROR_USER_CANCELED
		endif
		matchloc -= 1
	endif

	switch (matchloc)
		case 1:
			matchstr = matchstr + "*"
			break
		case 2:
			matchstr = "*" + matchstr
			break
		case 3:
			matchstr = "*" + matchstr + "*"
			break
		default:
	endswitch

	string wl = getwavelist(matchstr,1,TRUE)
	string wn
	variable wln,i

	wln = itemsinlist(wl)
	for(i=0;i<wln;i+=1)
		wn = stringfromlist(i,wl,";")

		killwave(wn,TRUE,TRUE)
	endfor
	return ERROR_SUCCESS
End
Override Function ExportExcel(wn,sfn,lmode)
											//exports waves to Microsoft Excel format files.
											//call: ConcatWave, GetWaveList, GetRecentPath
	string wn								//waveNameOrPattern
											//is the name or pattern of target wave(s) to be exported.
	string sfn								//saveFileName
											//is the file name to which the waves will be exported to.
	variable lmode							//launchMode
											//deciedes whether the exported file will be opened by MS Excel
											//_program or not.

	if((strlen(wn)==0)||(strlen(sfn)==0))		//prompt dialog to specify waves to export and save file
		prompt wn,"Wave name:"
		prompt sfn,"Save file name:"
		doprompt "Select waves to export",wn,sfn
		if(v_flag)
			return ERROR_USER_CANCELED
		endif

	endif

	if(strlen(wn)==0)
		wn = "*"
	endif
	if(strlen(sfn)==0)
		string/g fn
		sfn = replacestring("*",wn,"")
		if(strlen(fn))
			sfn = fn[0,strlen(fn)-4]+"_"+sfn+".xls"
		else
		endif
	endif
	if(cmpstr(parsefilepath(4,sfn,":",0,0),"xls"))
		sfn = sfn+".xls"
	endif

	if(strsearch(sfn,":",0)==-1)
		string path = GetRecentPath(ALL,TRUE)
		sfn = path+sfn
//		sfn = ParseFilePath(5, sfn, "\\", 0, 0)
//		sfn = replacestring(":",sfn,"\\")
//		sfn = replacestring("\\",sfn,":\\",0,1)
	endif
//	print sfn
	string wl = ""							//wave list
	string pl = ""							//prefix list

//	print wn
	variable i,n,j,m
	wl = GetWaveList(wn,0,TRUE)
	if(strlen(wl)==0)
		print "No wave has been selected. Exporting terminated."
		return ERROR_NO_TARGET_WAVE
	endif


	pl = ""

	n = itemsinlist(wl)
	string fd = "proctmp"
	if(datafolderexists("root:"+fd))	//kill data folder if exist
		killdatafolder root:$fd
	endif
	newdatafolder root:$fd
	m = 0
	for(i=0;i<n;i+=1)
		wn = stringfromlist(i,wl)


		if(dimsize($wn,1))
			for(j=0;j<dimsize($wn,1);j+=1)
				pl += wn+"["+num2str(j)+"];"
			endfor
		else
			pl += wn+";"
		endif
		m = max(m,dimsize($wn,0))
		if(wavetype($wn)==0)
			duplicate $wn,root:$(fd):$wn
		else
			make/t/n=(numpnts($wn)) root:$(fd):$wn
			wave/t wt = root:$(fd):$wn
			wave w = $wn
			wt = num2str(w)
		endif
	endfor
//	print m
	for(i=0;i<n;i+=1)
		wn = stringfromlist(i,wl)
		wave/t wt = root:$(fd):$wn
	//	print waveexists(wt),wn
		insertpoints dimsize(wt,0),m-dimsize(wt,0),wt
	endfor

	setdatafolder root:$(fd)
	i = strsearch(sfn,"\\",inf,1)
	wn = "exxls"
//	if(grepstring(wn,"^[A-z]")==0)
//		wn = "e"+wn
//	endif
	ConcatWave("*",3,FALSE,TRUE,wn)

	wave/t wt = $wn


	insertpoints 0,1,wt
	for(i=0;i<dimsize(wt,1);i+=1)
		wt[0][i] = stringfromlist(i,pl)
	endfor
//	print sfn,wn
//	edit $wn
	n = dimsize(wt,0)
	m = dimsize(wt,1)
//	print n
//	print wt
	for(i=0;i<n;i+=1)
		for(j=0;j<m;j+=1)
			if(grepstring(wt[i][j],"(?i)NaN"))
				wt[i][j] = ""
			endif
		endfor
	endfor
	Save/G $wn as sfn
//	abort
//	movefile/o/z sfn+".txt" as sfn

	setdatafolder root:
	killdatafolder root:$fd
//	pl = replacestring("\r",pl,"\r\n")
//	n = strlen(pl)
//	print sfn
	print "Selected waves exporting to file \""+sfn+"\" finished."
	if(lmode)
		ExecuteScriptText/b "excel \""+sfn
	endif
//	string line = ""
//	variable ref
//	Open ref as sfn
//	if (ref == 0)
//		print "File I/O Error."
//		return //-1
//	endif
//	pl = ""
//	FSetPos ref,0
//	do
//		FReadLine/n=1000/t="" ref, line
//		if (strlen(line)==0)
//			break
//		endif
//		pl = pl + line
//	while (1)
//	pl = replacestring("\r",pl,"\r\n")
//	FSetPos ref,0
//	fprintf ref,""
//	FSetPos ref,0
//	n = strlen(pl)
//	for(i=0;i<n;i+=1000)
//		FSetPos ref,i
//		line = pl[i,min(i+999,strlen(pl)-2)]
//
//		fprintf ref,line
//	endfor
//	fprintf ref,pl
//	close ref
	return ERROR_SUCCESS
End
Override Function MakeMnReg()
											//makes register text wave to hold mEPSC wave name information.
											//details: Because most of operations and functions which deal with
											//_mEPSC data need the name and / or time information, the reserved
											//_waves must exist. However, when importing saved mEPSCs from files
											//_these waves may not be available. In this case, the reserved waves
											//_need to be rebuilt.
											//call: KillWave
	string wl
	string minilpref
	wave/t rn
	minilpref = rn[9][1]

	variable i,n
	wl = wavelist(minilpref,";","")
	n = itemsinlist(wl)


	if((waveexists($"mnname")==0)||(numpnts($"mnname")!=n))
		make/t/o/n=(n) mnname
	endif
	wave/t mnname = $"mnname"

	if((waveexists($"mntime")==0)||(numpnts($"mntime")!=n))
		make/o/n=(n) mntime
	endif
	if(n)
		variable sf
		variable dx = deltax($stringfromlist(0,wl))
		sf = 1/dx
		for(i=0;i<n;i+=1)
			mnname[i] = stringfromlist(i,wl,";")
		endfor
		n = min(50,n)
		make/o/n=(n) tmplx,tmprx
		for(i=0;i<n;i+=1)
			tmplx[i] = leftx($mnname[i])
			tmprx[i] = rightx($mnname[i])+dx
		endfor
		variable prept,postpt
		prept = round(abs(mean(tmplx))*1000)/1000*sf
		postpt = round(abs(mean(tmprx))*1000)/1000*sf-1
		//		print prept,postpt,sf
		if(waveexists($"expinfo"))
			wave exw = $"expinfo"
			if((prept>0)&&(prept<inf))
				exw[12] = prept
			endif
			if((postpt>0)&&(postpt<inf))
				exw[13] = postpt
			endif
		endif
		killwave("tmplx;tmprx;",TRUE,TRUE)
	endif
	return ERROR_SUCCESS
End
Override Function UniLength(wn,fmode)
											//adjusts the start and end time point of target
											//_mEPSC wave.
	string wn								//waveName
											//is the name of target wave.
	variable fmode							//fillMode
											//is used to decide the method to set the values
											//_of newly inserted points.
	string wl
	variable i,n
	wave/t rn
	if(cmpstr(wn,rn[9][1])==0)
		wl = wavelist(wn,";","")
		n = itemsinlist(wl)
		for(i=0;i<n;i+=1)
			wn = stringfromlist(i,wl)
			UniLength(wn,fmode)
		endfor
		return ERROR_SUCCESS
	endif
//	if(stringmatch(wn,rn[9][1]))
	//	print wn
		wave exw = $"expinfo"
		variable prept,postpt,len,stlen,lx,tx
		prept = exw[12]
		postpt = exw[13]

		variable/g sf
		wave w = $wn
		stlen = prept+postpt+1
		len = numpnts(w)
	//	print wn,len,stlen
		if(len!=stlen)
	//		print "set length",wn
			lx = round(abs(leftx(w))*sf)
//			print lx
			if(lx!=prept)
				if(lx<prept)
//					tx = max(prept-lx+mod(prept-lx,2),2)
//					wavestats/q/m=1/r=[0,max(0.5*lx,2)] w
//					make/o/n=(tx) tmpm = v_avg+gnoise(v_sdev)
//					fft tmpm
//					tmpm[x2pnt(tmpm,5000),inf]=0
//					ifft tmpm
//					insertpoints 0,prept-lx,w
//				//	wave tmpm
//					w[0,tx-1] = tmpm
					insertpoints 0,prept-lx,w
					if(fmode)
						w[0,prept-lx-1] = w[prept-lx]
					else
						w[0,prept-lx-1] = NaN
					endif
				else
					deletepoints 0,lx-prept,w
				endif
				setscale/p x,-prept/sf,1/sf,"s",w
			endif

			n = numpnts(w)
			if(n!=stlen)
				if(n<stlen)
//					tx = max(stlen-n+mod(stlen-n,2),2)
//					wavestats/q/m=1/r=[x2pnt(w,0.8*rightx(w)),inf] w
//					make/o/n=(tx) tmpm = v_avg+gnoise(v_sdev)
//					fft tmpm
//					abort
//					tmpm[x2pnt(tmpm,5000),inf]=0
//					ifft tmpm
//
//
//					insertpoints n,stlen-n,w
//				//	wave tmpm
//					w[stlen-tx,stlen-1] = tmpm
					insertpoints n,stlen-n,w
					if(fmode)
						w[n,stlen-1] = w[n-1]
					else
						w[n,stlen-1] = NaN
					endif
				else
					deletepoints stlen,n-stlen,w
				endif
			endif
		endif
//	endif
	return ERROR_SUCCESS
End
Override Function ShowWave(wn,winpos)
											//displays wave, sets graph window name, size and position.
											//details: The graph window will be named as wn+"g" if the target
											//_wave wn exists. Otherwise, the displayed window name will be just wn
											//_(mainly for plot windows).
											//call: KillWindows
	String wn								//waveName
											//is the name of target wave.
	variable winpos						//posIndex
											//is the index of position where the graph will be displayed.
	if(strlen(wn)==0)
		return ERROR_INVALID_WAVE
	endif
	variable/g sf

	String wng =  replacestring(".",wn,"p")		//graph window name
	variable winleft,wintop,winright,winbottom
	switch (winpos)						//set position parameters
		case 1:
			winleft = 10
			wintop = 0
			winright = 310
			winbottom = 160
			break
		case 2:
			winleft = 320
			wintop = 0
			winright = 620
			winbottom = 160
			break
		case 3:
			winleft = 10
			wintop = 240
			winright = 310
			winbottom = 400
			break
		case 4:
			winleft = 320
			wintop = 240
			winright = 620
			winbottom = 400
			break
		case 5:
			winleft = 630
			wintop = 0
			winright = 930
			winbottom = 160
			break
		case 6:
			winleft = 630
			wintop = 240
			winright = 930
			winbottom = 400
			break

		default:

			winleft = 22*(winpos-10)
			wintop = 44+22*(mod(winpos,10))
			winright = 300 + winleft
			winbottom = 160 + wintop

	endswitch
	if(waveexists($wn))
		wng = replacestring(".",wn,"p") +"g"
		display/w=(winleft,wintop,winright,winbottom) $wn as wn

		if(strlen(winlist(wng,";","WIN:7"))!=0)
			killwindows(wng,"",1)
		endif
		DoWindow/C $lowerstr(wng)
		wavestats/m=1/q $wn
		variable posaxis = v_max + 0.1*(v_max - v_min)
		variable negaxis = v_min - 0.1*(v_max - v_min)
	//	if(datatype==1)

		SetAxis /a/w=$wng bottom, leftx($wn), rightx($wn)
		SetAxis /w=$wng left, negaxis, posaxis

	//	endif
	//	SetScale y,0,0,"A",$wn

		ModifyGraph /w=$wng grid(bottom)=2
		ModifyGraph /w=$wng grid(left)=2
	else
		display/w=(winleft,wintop,winright,winbottom) as wn
		if(strlen(winlist(wng,";",""))!=0)
			killwindows(wng,"",1)
		endif
		DoWindow/C $lowerstr(wng)

	endif
	return ERROR_SUCCESS
End
Override Function ShowTable(tn,winpos)
											//shows table and sets its position.
	string tn								//tableName
											//is the name of table which is to be shown.
	variable winpos						//posIndex
											//is the index of position that the target table window
											//_will be displayed at.
	string tl,wl,wn
	variable winleft,wintop,winright,winbottom
	variable coln,rown,i,n,width,height
	if(winpos<0)
		tl = winlist("*",";","WIN:2")
		n = itemsinlist(tl)
//		winpos = WhichListItem(tn,tl,";",0,0)+30
		winpos = n
//		print tl,tn
//		print WhichListItem(tn,tl,";",0,0)
	endif
//	print winpos
	winpos+=30
	wl = wavelist("*",";","Win:"+tn)
	n = itemsinlist(wl)
	coln = 0
	rown = 0
	for(i=0;i<n;i+=1)
		wn = stringfromlist(i,wl)
		coln += max(dimsize($wn,1),1)
		rown = max(rown,dimsize($wn,0))
	endfor
//	print coln,rown

	width = max(80 + coln * 83,210)
	height = min(60+rown*20,300)

	winleft = 22*(winpos-10)

	wintop = 44+22*(mod(winpos,10))
	winright = min(winleft + width,1000)
	winbottom = min(wintop + height,400)

//	print winleft,wintop,winright,winbottom
	movewindow/w=$tn winleft,wintop,winright,winbottom
	dowindow/f $tn
	return ERROR_SUCCESS
End
Override Function CleanTemp(cmode)
											//kills temporary waves used during data processing.
											//call: KillWave, KillWindows
											//seealso: ScanOrg
	variable cmode							//cleanMode
											//is the flag to decide whether the cleaning should be
											//_proceeded or not. In debugging mode, cmode will be set
											//_to 0 and no temporary waves will be killed.
	if(cmode)
		if(exists("a")==2)
			killvariables a
		endif
		killwave("dw",TRUE,TRUE)
		killwave("ow",TRUE,TRUE)
		killwave("bw",TRUE,TRUE)
		killwave("sw",TRUE,TRUE)
		killwave("pamp",TRUE,TRUE)
		killwave("tmpm",TRUE,TRUE)
		killwave("tmp*",TRUE,TRUE)
		killwave("mt*",TRUE,TRUE)
		killwave("mipw",TRUE,TRUE)
		killwave("charge",TRUE,TRUE)
		killwave("w_coef",TRUE,TRUE)
		killwave("w_sigma",TRUE,TRUE)
		killwindows("Set channel configuration;Show channel configuration;Input AD name:;Input Data type:;Input Gain value:;Pause for Judge;Pause for Cursor;mEPSC judging progress","",2)
	endif
	return ERROR_SUCCESS
End
Override Function/S GetResWaveList(lmode)
										//returns the list string of reserved wave names.
										//details: The returned value will be semi-colon separated string
										//_if target waves have been found or "" vice versa.
	variable lmode						//checkMode
										//is used to specify the method to find reserved wave names. The
										//_value could be:
										//  FALSE    All names of reserved waves will be collected.
										//  TRUE    Return value contains only names of existed reserved wave.
	string wl="rn;rnw;rchmap;expinfo;exhis;exerr",rl=""
	variable i
	if(lmode)
		for(i=0;i<6;i+=1)
			if(waveexists($stringfromlist(i,wl)))
				rl += stringfromlist(i,wl)+";"
			endif
		endfor
		wl = rl
	endif
	return wl
End
Override Function CleanUp()
											//is used when data input process (from file) will be performed
											//_while extracted and / or calculated waves are already existing
											//_in the experiment.
											//dedtails: It kills all objects, or moves them to specified data folder,
											//_or do nothing before loading waves from new data file, according to
											//_user's decision.
											//call: CheckInit, GetResWaveList, KillWave, KillWindows
	string wl = removefromlist(GetResWaveList(TRUE),wavelist("*",";",""))
	variable ci
	variable n = itemsinlist(wl)
	if (n)			//Old data exist
		if(waveexists($"def_value"))
			wave w = $"def_value"
			ci = w[0]
		else
			string kstr = "Old data exists. Delete all?\rClick \"Yes\" to kill all old data.\r"
			kstr += "Click \"No\" to move objects to data folder.\r"
			kstr += "Click \"Cancel\" to do nothing."
			DoAlert/t="CleanUp" 2, kstr
			ci = v_flag
		endif
		switch(ci)
			case 3:
				return ERROR_USER_CANCELED
			case 2:		//clicked NO, move all objects to data folder

				break
			case 1:		//clicked YES, kill all objects
				killwindows("*","",0)
				killwave("*",TRUE,TRUE)
				if(CheckInit()==ERROR_SUCCESS)
					killvariables/a/z
					killstrings/a/z
				endif
				break
			default:
		endswitch
	endif
	return ERROR_SUCCESS
End
Override Function KillmEPSC(wn,mwi)
											//kills mEPSC waves, its register information and calculated properties.
											//details: KillmEPSC firstly searches the mEPSC register text wave for the
											//_given wn. If it is valid, the mEPSC of this name will be killed. If the
											//_function falis to find the named mEPSC, it will try to remove mEPSC indexed as mwi.
											//_When the above two steps still don't work and the wn is an empty string,
											//_a dialog for specifying the mEPSC will be provided. Function will do
											//_nothing if both the name and the index are wrong.
											//After the operation is done, the time and name information of the removed
											//_wave will be unregistered. The calculated property values will also been
											//_deleted if exist.
											//call: KillWave
	String wn								//waveName
											//is the name of the target wave.
	variable mwi							//waveIndex
											//is the index of the mEPSC in the register list.

	String minipref,minilpref,capref
	wave/t rn
	minipref = rn[9][0]
	minilpref = rn[9][1]
	capref = rn[11][0]

	variable i
	wave mntime = $"mntime"
	wave/t mnname = $"mnname"

	wave amp
	variable pt = numpnts(mntime)
	FindValue/txop=2/text=wn mnname
	if(v_value==-1)								//bad wn
		if((mwi<0)||(mwi>pt-1))					//index also out of range
			if(strlen(wn))							//wrong name and wrong index
				Print "No mEPSC removed."
				return ERROR_INDEX_OUT_OF_RANGE
			else									//empty name for prompt
				Prompt wn,"Miniature Wave Name:"//,popup,wavelist(minilpref,";","TEXT:0")
				Prompt mwi,"Miniature Wave Index:"
				doprompt "Choose Miniature to Kill:",wn,mwi
				if(v_flag==0)						//continued
					killmepsc(wn,mwi)
					return ERROR_SUCCESS
				else								//cancalled
					return ERROR_USER_CANCELED
				endif
			endif

		else										//valid mwi
			wn = mnname[mwi]
		endif
	else
		mwi = v_value
	endif

//	print "miniwn
	variable r=0
//	print wn
//	print mwi
	r = Killwave(wn,TRUE,TRUE)
	string swn,fswn
	swn = "mnscale"
	fswn = "mnfit"
	String plotsmini,plotfsmini
	wave/t rnw
	plotsmini = rnw[1]
	plotfsmini = rnw[2]
	if(waveexists($swn))
		deletepoints/m=1 mwi,1,$swn
		if(strlen(winlist(plotsmini,";","")))
			if(numpnts(mnname)-1)
				removefromgraph/w=$plotsmini $swn+"#"+num2str(numpnts(mnname)-1)
			else
				removefromgraph/w=$plotsmini $swn
			endif
		endif
	endif
	if(waveexists($fswn))
		deletepoints/m=1 mwi,1,$fswn
		if(strlen(winlist(plotsmini,";","")))
			if(numpnts(mnname)-1)
				removefromgraph/w=$plotfsmini $fswn+"#"+num2str(numpnts(mnname)-1)
			else
				removefromgraph/w=$plotfsmini $fswn
			endif
		endif
	endif
	string wnc = ""
	wnc = capref+wn[strlen(minipref),inf]
	if(waveexists($wnc)==1)
//		print "killwave:",wnc
		Killwave(wnc,TRUE,TRUE)
	endif

//	n = mntime[mwi]

//	print "!!!",n,miniwi,mntime[miniwi],mntime[miniwi]
	if(r==ERROR_SUCCESS)
		deletepoints mwi,1, mntime,mnname

		wave/t rn = $"rn"
		for(i=1;i<9;i+=1)
			if(waveexists($rn[i][0]))
				deletepoints mwi,1,$rn[i][0]
			endif
		endfor
	else
		print "KillWave Error. Wave "+wn+" Not Killed."
	endif
	return ERROR_SUCCESS
End
Override Function KillSingle()
											//kills mEPSC or presynaptic waves if pair data not exists.
											//call: KillWave
	variable/g datatype
	if(datatype&4)
		print "This Function is for Double Patch Data Only."
		return ERROR_NO_TARGET_WAVE
	endif
	string wl,wlc
	variable i,m,n
	string minipref,capref,minilpref,calpref
	wave/t rn
	minipref = rn[9][0]
	capref = rn[11][0]
	minilpref = rn[9][1]
	calpref = rn[11][1]

	string wn,wnc
	wl = wavelist(minilpref,";","")
	wlc = wavelist(calpref,";","")
	m = itemsinlist(wl)
	for(i=0;i<m;)
		wn = stringfromlist(i,wl,";")


		wnc = capref + wn[strlen(minipref),inf]
		if(waveexists($wnc))
			wl = removefromlist(wn,wl)
			wlc = removefromlist(wnc,wlc)
			m -= 1
		else
			i += 1
		endif
	endfor

	for(i=0;i<m;i+=1)
		wn = stringfromlist(i,wl,";")
		killwave(wn,TRUE,TRUE)
	endfor

	n = itemsinlist(wlc)

	for(i=0;i<n;i+=1)
		wnc = stringfromlist(i,wlc,";")
		killwave(wnc,TRUE,TRUE)
	endfor
	if((m!=0)||(n!=0))
		print "Orphan Waves: "+wl+wlc+" Are Deleted."
	endif
	return ERROR_SUCCESS
End
Override Function ModifyWave(winn,wn,linecolor,linesize)
											//modifies specific wave appearance on certain graph.
											//call: GetColor
	String winn								//windowName
											//is the name of window on which displayed the target wave.
	string wn								//waveName
											//is the name of target wave.
	variable linecolor						//colorIndex
											//is the index of color which is to be applied to the
											//_target wave.
	variable linesize						//lineSize
											//is the size (thickness) of line will be used.

	variable r,g,b
											//switch(color) may be replaced by strswitch(colorstr) in next version
	GetColor(linecolor,r,g,b)
//print color
	String windowlist = winlist(winn,";","Win:7")
	variable windowln = ItemsInList(windowlist)
	variable i
	if((windowln>0)&&(cmpstr(wn,"*")==0))
		string wl = wavelist("*",";","WIN:"+winn)

		for(i=0;i<ItemsInList(wl);i+=1)
			ModifyWave(winn,StringFromList(i,wl,";"),linecolor,linesize)
		endfor
		return ERROR_SUCCESS
	endif
	i = strsearch(wn,"#",0)
	string twn
	if(i==-1)
		twn = wn
	else
		twn = wn[0,i-1]
	endif
//	string twn = wn[0,strsearch(wn,"#",0)-1]
	if((waveexists($twn))&&(windowln>0))
		ModifyGraph /W=$winn lsize($wn)=linesize,rgb($wn)=(r,g,b)
	endif
	return ERROR_SUCCESS
End
Override Function GetColor(linecolor,rx,gx,bx)
											//sets values of red, green and blue component of the color
											//_according to the given linecolor.
											//details: The rx, gx and bx will be set after the successful
											//_execution. The arguments are passed by reference by the
											//_calling functions. Thus, GetColor can be ONLY used in the
											//_programs exclusively.
											//seealso: How Parameters Work, ModifyGraph for Colors
	variable linecolor						//colorIndex
											//is the numeric index of the colors listing as follows:
											//  -1    Black.
											//  0    Red.
											//  1    Orange.
											//  2    Green.
											//  3    Blue.
											//  4    Purple.
											//  5    Magenta.
											//  6    Pink.
											//  7    Cyan.
											//  8    Light blue.
											//  9    Green blue.
											//  10    Dark green.
											//  11    Dark red.
											//  12    Brown.
											//  13    Yellow.
											//  Other    	Gray.
	variable &rx							//redComp
											//is an integer from 0 to 65535 for color red component.
	variable &gx							//greenComp
											//is an integer from 0 to 65535 for color green component.
	variable &bx							//blueComp
											//is an integer from 0 to 65535 for color blue component.
	switch (linecolor)
	case -1:		//black
		rx=0
		gx=0
		bx=0
		break
	case 0:			//red
		rx=65280
		gx=0
		bx=0

		break
	case 1:			//orange
		rx=65280
		gx=43520
		bx=0
		break
	case 2:			//green
		rx=0
		gx=65280
		bx=0
		break
	case 3:			//blue
		rx=0
		gx=0
		bx=65280
		break
	case 4:			//purple
		rx=29440
		gx=0
		bx=58880
		break
	case 5:			//magenta
		rx=65280
		gx=0
		bx=52224
		break
	case 6:			//pink
		rx=65280
		gx=48896
		bx=62208
		break
	case 7:			//cyan
		rx=0
		gx=65280
		bx=65280
		break
	case 8:			//light blue
		rx=0
		gx=43520
		bx=65280
		break
	case 9:			//green blue
		rx=0
		gx=65280
		bx=33024
		break
	case 10:		//dark green
		rx=0
		gx=39168
		bx=0
		break
	case 11:		//dark red
		rx=39168
		gx=0
		bx=15616
		break
	case 12:		//brown
		rx=39168
		gx=26112
		bx=0
		break
	case 13:		//yellow
		rx=65280
		gx=65280
		bx=0
		break
	default:		//gray
		rx=32768
		gx=32768
		bx=32768
	endswitch
	return ERROR_SUCCESS
End
Override Function PeakAlignment(wn)
											//aligns mEPSC waves according to amplitude peak time points.

											//call: RecHistory
	string wn
	variable/g datatype
	if(datatype&1)
		wave mntime = $"mntime"
		wave/t mnname = $"mnname"
		if(waveexists(mntime)==0)
			return ERROR_NO_TARGET_WAVE
		endif
		variable i
		if(cmpstr(wn,"*")==0)
			make/o/n=(NumberByKey("N_PARAMS", functioninfo(GetRTStackInfo(1))))/t tmpexhis = {wn}
			RecHistory()
			variable wln = numpnts(mntime)
			for (i=0;i<wln;i+=1)
				PeakAlignment(mnname[i])
			endfor
			return ERROR_SUCCESS
		endif

		variable/g expsign
		variable px
		if(waveexists($wn)==0)
			return ERROR_INVALID_WAVE
		endif
		wavestats/q/m=1 $wn
		if(expsign==1)
			px = v_maxloc
		else
			px = v_minloc
		endif

		variable dx = deltax($wn)
		setscale/p x,leftx($wn)-px,dx,"s",$wn
		string swn,fswn
		swn = "scaled_"+wn
		fswn = "fit_scaled_"+wn
		if(waveexists($swn))
			dx = deltax($swn)
			setscale/p x,leftx($swn)-px,dx,"s",$swn
		endif
		if(waveexists($fswn))
			dx = deltax($fswn)
			setscale/p x,leftx($fswn)-px,dx,"s",$fswn
		endif
		if(datatype&4)
			string wnc
			wave/t rn
			wnc = rn[11][0]+wn[strlen(rn[9][0]),inf]
			dx = deltax($wnc)
			setscale/p x,leftx($wn)-px,dx,"s",$wnc
		endif
	endif
	return ERROR_SUCCESS
End
Override Function DiffAlignment(wn)
											//aligns mEPSC waves according to onset differential peak time points.

											//call: RecHistory
	string wn
	variable/g datatype
	if(datatype&1)
		wave mntime = $"mntime"
		wave/t mnname = $"mnname"
		if(waveexists(mntime)==0)
			return ERROR_NO_TARGET_WAVE
		endif
		variable i
		if(cmpstr(wn,"*")==0)
			make/o/n=(NumberByKey("N_PARAMS", functioninfo(GetRTStackInfo(1))))/t tmpexhis = {wn}
			RecHistory()
			variable wln = numpnts(mntime)
			for (i=0;i<wln;i+=1)
		//	string/g
				DiffAlignment(mnname[i])
			endfor
			return ERROR_SUCCESS
		endif

		variable/g sf
		if(waveexists($"expinfo")==0)
			return ERROR_INVALID_RES_WAVE
		endif
		wave exw = $"expinfo"
		variable prept = exw[12]
		variable postpt = exw[13]
		variable px
		if(waveexists($wn)==0)
			return ERROR_INVALID_WAVE
		endif
		duplicate/o $wn tmpm
		tmpm = abs(tmpm)
		variable smd = 1e-3*sf
		smooth smd,tmpm
		differentiate tmpm/d =tmp
		wavestats/q/m=1 tmp
		px = v_maxloc

		variable dx = deltax($wn)
		setscale/p x,leftx($wn)-px,dx,"s",$wn
		string swn,fswn
		swn = "scaled_"+wn
		fswn = "fit_scaled_"+wn
		if(waveexists($swn))
			dx = deltax($swn)
			setscale/p x,leftx($swn)-px,dx,"s",$swn
		endif
		if(waveexists($fswn))
			dx = deltax($fswn)
			setscale/p x,leftx($fswn)-px,dx,"s",$fswn
		endif
		if(datatype&4)
			string wnc
			wave/t rn
			wnc = rn[11][0]+wn[strlen(rn[9][0]),inf]
			dx = deltax($wnc)
			setscale/p x,leftx($wn)-px,dx,"s",$wnc
		endif
	endif
	return ERROR_SUCCESS
End
Override Function/S FolderList(tfd,fn)
												//returns the list of folders in tfd.
	string tfd									//targetFolder
												//is the name string of data folder in which the
												//_sub folders will be checked. When no data folder
												//_has been found, the return value will be "".
	string fn									//folderName
												//is the name pattern of folders to be examined.
	string fl = ""
	dfref cfd = GetDataFolderDFR()

	if(strlen(tfd))
		if(cmpstr(tfd[strlen(tfd)-1],":"))
			tfd+=":"
		endif
		if((stringmatch(tfd,"root:*")==0)&&(cmpstr(tfd[0],":")))
			tfd = ":"+tfd
		endif
		if(datafolderexists(tfd))
			setdatafolder $tfd
		else
			return fl
		endif
	endif
	fn = replacestring(",",fn,";")
	variable i,n
	string fstr="",sl=""
	n = itemsinlist(fn)
	if(n>1)
		for(i=0;i<n;i+=1)
			fstr = stringfromlist(i,fn)
			sl = FolderList("",fstr)
			fl = removefromlist(sl,fl)
			fl += sl
		endfor
	else
		fl = datafolderdir(1)
		fl = replacestring(",",fl,";")
		fl = replacestring("\r",fl,"")
		fl = replacestring("FOLDERS:",fl,"")
		fl = ListMatch(fl,fn)
	endif
	setdatafolder cfd
	return fl
End
Override Function/S GetWaveList(pstr,smode,rck)
											//returns name list of waves selected by the name(s)
											//_or pattern(s) list(s) or the combination of above.
											//details:
											//seealso: WaveList, GrepString, GrepList, Regular Expressions
	string pstr								//patternStr
											//is the pattern string used to select target wave(s).
	variable smode							//sortMethod
											//is a flag used to decided which sorting method will be
											//_applied to the output list string. It is reserved for
											//_future use and should be passed as 0.
	variable rck							//redundantCheck
											//is used to check and remove the replicated names in the
											//_list. The value could be:
											//  FALSE    Ignore the replicated names (if exist) in output list string.
											//  TRUE    Remove all the redundant names.
	string wl = ""
	string rl = ""
//	print pstr
	pstr = replacestring(",",pstr,";")
	pstr = replacestring("\t",pstr,";")
	pstr = replacestring("\r",pstr,";")
//	pstr = replacestring("(",pstr,"[")
//	pstr = replacestring("(",pstr,"]")

	variable i,n,j,m,k,o,l,v
	string estr	,sstr						//each, sub pattern
	string wnp,wn							//wave name & name pattern
	string wcp,wc							//wave class & class pattern
	string wt								//wave text type
	string winl,winn,winp					//window list / name / pattern
	string regex = ""
	n = itemsinlist(pstr)
	for(i=0;i<n;i+=1)						//check each pattern
		estr = stringfromlist(i,pstr)
		m = itemsinlist(estr,"[")
		wnp = ""
		winp = ""
		wcp = ""
		wt = ""
		wl = ""
		for(j=0;j<m;j+=1)
			sstr = stringfromlist(j,estr,"[")		//check each type info

			if(strsearch(sstr,"]",0)==-1)			//wave name pattern
				if(strlen(wnp)==0)
					wnp = sstr
				endif
			else									//window or type pattern
				sstr = replacestring("]",sstr,"")
				if((strsearch(sstr,"win:",0,2)>-1)&&(strlen(winp)==0))	//window name pattern
					winp = sstr[strsearch(sstr,"win:",0,2),inf]

				elseif((strsearch(sstr,"property:",0,2)>-1)&&(strlen(wcp)==0))
					wave/t rn
					if(strlen(sstr)==9)
						for(k=1;k<9;k+=1)
							wcp += rn[k][0] + ";"
						endfor
//						for(k=13;k<15;k+=1)
//							wtp += rn[k][0] + ";"
//						endfor
					else
						wc = sstr[9,inf]
						if(grepstring(wc,"^\d"))
							k = str2num(wc[0,inf])
							wcp = rn[k][0] + ";"
						endif
					endif
				elseif((strsearch(sstr,"text:",0,2)>-1)&&(strlen(wt)==0))
					if(strsearch(sstr,"text:1",0,2)>-1)
						wt = ",text:1"
					else
						wt = ",text:0"
					endif
				endif
			endif
		endfor
//		print wcp,wnp
		if(strlen(wl)==0)

			if((strlen(wnp)==0)&&((strlen(winp)||strlen(wcp)||strlen(wt))))
				wnp = "*"
			endif
	//		print wnp
			if(strlen(wcp))
				wc = ""
				v = itemsinlist(wnp)
				for(l=0;l<v;l+=1)
					wn = stringfromlist(l,wnp)

					wc = replacestring(";",wcp,wn+";")
				endfor
				wnp = wc
			endif
	//		print wnp
			v = itemsinlist(wnp)

			if(grepstring(winp,"(?i)win:\d$"))		//window type
	//		print winp
				winl=winlist("*",";",winp)
		//		print wl
				if(strlen(winl))
					winl = "win:"+replacestring(";",winl[0,strlen(winl)-2],";win:")
				endif
			else
				if(strlen(winp)>4)					//window name
	//				print winp
					winl=winlist(winp[4,inf],";","WIN:7")
	//				print winl
					if(strlen(winl))
						winl = "win:"+replacestring(";",winl[0,strlen(winl)-2],";win:")
					endif
				else								//target window
	//				print "!!!win:"
	//				print winp
					winl = winp
				endif
			endif
			if(strlen(winl)==0)
				winl = ";"
			endif
	//		print "xxxxxxxxx",winl,wnp

			for(l=0;l<v;l+=1)
				regex = ""
				wn = stringfromlist(l,wnp)
				if(strsearch(wn,"\\",0)>-1)
					if(cmpstr(wn[0],"!"))
						regex = wn[0,strsearch(wn,"\\",0)-1] + "*"
					else
						regex = "*"
					endif
				else
					regex = wn
				endif
				o = itemsinlist(winl)
				for(k=0;k<o;k+=1)
					winn = stringfromlist(k,winl)
				//	print "wn=",regex,"winn=",winn
					wl = wavelist(regex,";",winn+wt)
				//	print regex,wn
					if(strsearch(wn,"\\",0)>-1)
						variable sp = strsearch(wn,"\\",0)
						if(cmpstr(wn[0],"!"))
							regex = "^(?i)" + replacestring("*",wn[0,sp-1],"\S*")+wn[sp,inf]+"$"
					//		regex = replacestring("\\D",wn,"")
					//		print regex
					//		print regex,greplist(wl,regex)
							wl = greplist(wl,regex)
						else
							regex = "^(?i)" + replacestring("*",wn[1,sp-1],"\S*")+wn[sp,inf]+"$"
					//		regex = replacestring("\\D",wn,"")
						//	print regex,wn
//							print "!!!!"
//							print regex,greplist(wl,regex)
//							print "!!!!!1"
							wl = removefromlist(greplist(wl,regex),wl,";",0)
						endif
					endif
	//				print winn
					winn = winlist("*","",winn)
	//				print winn,wintype(winn)==2,wintype(winn)
					if(wintype(winn)!=2)
						rl += wl
					else
						string sl = wl
						variable col = 0
						wl = ""
//						m = itemsinlist(sl)
						for(j=0;;)
							wn = NameOfWave(WaveRefIndexed(winn,j,1))
		//					print wn
							if(strlen(wn))
								if(WhichListItem(wn,sl,";",0,0)>-1)
									wl += wn + ";"
								endif
							else
								break
							endif
							j += max(1,dimsize($wn,1))
						endfor
	//					print "wxxxxx",wl
						rl += wl
					endif
				endfor
			endfor
		endif
	endfor
//	sortlist()
	rl = lowerstr(rl)
	if(rck)
		n = itemsinlist(rl)
		wl = ""
		for(i=0;i<n;i+=1)
			wn = stringfromlist(i,rl)
			if(WhichListItem(wn,wl)==-1)
				wl += wn+";"
			endif
		endfor
	else
		wl = rl
	endif
	return wl
End
Override Function KillWave(wn,ckw,rk)
											//kills waves without on-graph or on-table restriction.
											//details: Target waves with name matches wn will be removed
											//_from graph or table firstly before killed. An error will
											//_be printed to history area if target wave is not able to
											//_be killed.
											//call: UnlinkWave, SendToDustBin
	string wn								//waveName
											//is the name, pattern, list, or combination of above of target
											//_wave(s) about to be killed.
	variable ckw							//checkWindow
											//is a flag to determine whether the empty window would be
											//_closed or not after the function execution.
											//  FALSE    Donot close window even there is no wave on it.
											//  TRUE    Close empty window after removing waves.
	variable rk								//killMode
											//is a switch to decide whether wave named wn will
											//_be deleted or moved to the dustbin folder.
											//  FALSE    Move wn to "Recycled" folder.
											//  TRUE    Kill specified wave(s).
	variable i,j,n
	string wl
	wl = replacestring(",",wn,";")		//compatible to list use comma as separator

	n = itemsinlist(wl)

	if(n>1)									//kill a list of waves
		for(i=0;i<n;i+=1)
			wn = stringfromlist(i,wl)
			killwave(wn,ckw,rk)
		endfor
		return ERROR_SUCCESS
	else
		wn = replacestring(";",wn,"")
	endif
	variable vp
	vp = strsearch(wn,"*",0)
	if(vp>-1)								//kill a pattern
	//	print wn
		wl = wavelist(wn,";","")
		n = itemsinlist(wl)
	//	print n
		for(i=0;i<n;i+=1)
			wn = stringfromlist(i,wl)

			killwave(wn,ckw,rk)
		endfor
		return ERROR_SUCCESS
	endif

	if(waveexists($wn)==0)					//return if wave not exist
		return ERROR_INVALID_WAVE
	endif
	if(rk)									//kill wave
		if(ckw)
			UnlinkWave(wn)					//remove from all windows
		endif
	//	print "wn=",wn

		variable err = getrterror(1)
		setwavelock 0,$wn
		killwaves $wn
		err = getrterror(1)
		if(err)
			print geterrmessage(err)
			print "Error: Wave \""+wn+"\" is not killed!"
//			print getrtstackinfo(2),wl
//			abort
			return ERROR_WAVE_NOT_KILLED
		endif
	else									//move to dustbin
		SendToDustBin(wn)
	endif
//	print "x"
	return ERROR_SUCCESS
End
Override Function UnlinkWave(wn)
											//removes the named wave from all window(s).
											//call: KillWindows
	string wn								//waveName
											//is the name of target wave which needs to be
											//_removed from window(s).

	if(waveexists($wn)==0)
		return ERROR_INVALID_WAVE
	endif
	string windowlist = winlist("*",";","WIN:7")
	string windowname
	if((waveexists($"rnw"))&&(waveexists($"rn")))
		wave/t rnw,rn
		if((stringmatch(wn,rn[9][1])==0)&&(stringmatch(wn,"*_"+rn[9][1])==0))
			windowlist = removefromlist(rnw[0]+";"+rnw[1]+";"+rnw[2],windowlist,";",0)
		endif
	endif
//	print windowlist
	variable wln,windowln,i,j,k
	string wl,tn
	windowln = ItemsInList(windowlist)
	for(i=0;i<windowln;i+=1)
		windowname = StringFromList(i,windowlist,";")
		if(wintype(windowname)==1)
			j = 0
			do										//remove ywaves if wn is an xwave
				tn = wavename(windowname,j,2)
				if(stringmatch(wn,tn))				//wn is an xwave. remove all ywave
					killwindows(windowname,"",1)
//					wl = TraceNameList(windowname,";",1)
//					wl = sortlist(wl,";",17)
//					wln = itemsinlist(wl)
//			//		print wl,windowname
//					for(k=0;k<wln;k+=1)
//						tn = stringfromlist(k,wl)
//						RemoveFromGraph/w=$windowname $tn
//					endfor
					break
				else
					if(strlen(tn))					//valid xwave but not wn
						j+=1
					else							//no more xwave, stop
						break
					endif
				endif
			while(1)

	//		print "j=",j,windowname,wn
			if(strlen(wavelist(wn,";","WIN:"+windowname)))
	//			print wn,windowname
				wl = TraceNameList(windowname,";",1)	//all traces including counter traces
				wl = greplist(wl,"^(?i)"+wn+"(#\d)*$")	//find wn and wn#d
				wl = sortlist(wl,";",17)				//descending sort
				wln = itemsinlist(wl)
				for(j=0;j<wln;j+=1)
					tn = stringfromlist(j,wl,";")
		//						print "wn="+wn,"win="+windowname,"tn="+tn,"tl="+TraceNameList(windowname,";",1)
					RemoveFromGraph/w=$windowname $tn
				endfor
			endif
		else
	//		print "xx"
			if(wintype(windowname)==2)
				wl = wavelist(wn,";","WIN:"+windowname)
				if(WhichListItem(wn,wl,";",0,0)!=-1)
		//		print windowname,wn
					RemoveFromTable/w=$windowname $wn
				endif
			endif
		endif
		if(itemsinlist(wavelist("*",";","WIN:"+windowname))==0)
			killwindows(windowname,"",1)
		endif
	endfor

	variable err = getrterror(1)
	if(err)
//		print geterrmessage(err),wn
//		print "xxx",getrtstackinfo(1),getrtstackinfo(2)
////		print "1,",windowln,",2,",wln,",3,",tn,",4,",wl,",5,",windowname
//		abort
	endif

	return ERROR_SUCCESS
End
Override Function KillWindows(winn,wn,wt)
											//kills windows by name patterns and window type.
											//seealso: WinList
	string winn								//winName
											//is the name, title string or pattern of target window(s)
											//_to close.
	string wn								//waveName
											//is the wave name used to choose target window.
	variable wt								//winType
											//is a flag for matching windows of certain types to kill.
											//  Bit 0:    Only graphs, tables and layouts are considered.
											//  Bit 1:    winn is window title string rather than name string.
	string wl = replacestring(",",winn,";")
	variable i,n,j,m
	n = itemsinlist(wl)
//	print n,wl
	if(n>1)
		for(i=0;i<n;i+=1)
			winn = stringfromlist(i,wl)
			KillWindows(winn,wn,wt)
		endfor
		return ERROR_SUCCESS
	endif
	string wlop = ""
	if((wt&0x01)==1)
		wlop = "WIN:7"
	endif
	string pwl = ""

	if((wt&0x2)==0)
	wl = winlist(winn,";",wlop)
	pwl = winlist(winn,";","WIN:128")
	wl = removefromlist(pwl,wl)
	else
		string wint = winn
		wl = winlist("*",";",wlop)
		pwl = winlist("*",";","WIN:128")
		wl = removefromlist(pwl,wl)
	endif
	if(NumberByKey("IGORVERS", IgorInfo(0))>=6.2)
		pwl = winlist("*",";","WIN:512")
		wl = removefromlist(pwl,wl)
	endif
//	print wl
	n = itemsinlist(wl)
//	print n
	string tn
	variable k,nk
	for(i=0;i<n;i+=1)
		winn = stringfromlist(i,wl)
		if((wt&0x2)==0)
			if(strlen(wn))
				k = 0
				m = itemsinlist(wn)
				for(j=0;j<m;j+=1)
					tn = stringfromlist(j,wn)
					if(itemsinlist(wavelist(tn,";","win:"+winn)))
						k = 1
						break
					endif
				endfor
			else
				k = 1
			endif
		else

			GetWindow $winn,title
//			if(cmpstr(wint,"Pause for judge")==0)
//				print winn,wint,s_value
//			endif
			if(cmpstr(s_value,wint)==0)
				k = 1
			else
				k = 0
			endif
		endif
		if(k)
			KillWindow $winn
			dowindow $winn
			if(v_flag)
				nk=1
			endif
		endif
	endfor
	if(nk)
		return ERROR_WINDOW_NOT_KILLED
	else
		return ERROR_SUCCESS
	endif
	return ERROR_SUCCESS
End
Override Function SendToDustBin(wn)
											//sends waves to dust bin data folder.
											//details: It is used for "Undo" process if "valuable"
											//_waves have to be deleted for some reasons. Target
											//_waves will be moved to a data folder named as "Recycled"
											//_under root data folder. Names of these waves will be
											//_saved in a register text wave.
											//call: UnlinkWave
											//seealso: KillWave
	string wn								//waveName
											//is the name of target wave to be safely deleted.

	if(waveexists($wn)==0)
		return ERROR_INVALID_WAVE
	endif
//	variable kew							//killEmptyWindow
	string fd = "recycled"					//recycle folder name and wave prefix
	SetDataFolder root:
	if(datafolderexists("root:"+fd)==0)	//create folder if not exist
		newdatafolder root:$fd
	endif
	setdatafolder root:$(fd):
	variable i = -1
	variable j,m
	if(waveexists($"deleteinfo")==0)			//make register wave if not exist
		make/t/o/n=(0,2) $"deleteinfo"
	endif

	wave/t rwn = $"deleteinfo"
	variable n = numpnts(rwn)/2
	string nwn = ""

	if(n)									//recycle wave name index (suffix)
		nwn = rwn[n-1][1]
		m = str2num(nwn[strlen(fd),inf])+1
	else
		m = 0
	endif
//	print m,nwn,wn
	nwn = fd + num2str(m)					//recycle wave name
	insertpoints n,1, rwn					//register original and formatted names
	rwn [n][0] = wn
	rwn [n][1] = nwn
	setdatafolder root:
	UnlinkWave(wn)							//remove wave from windows
	MoveWave $wn, root:$(fd):$nwn

	string minipref,minilpref				//update property waves to avoid mismatching
	string epscpref,epsclpref
	if(waveexists($"rn")==0)
		return ERROR_INVALID_RES_WAVE
	endif
	wave/t rn
	minipref = rn[9][0]
	minilpref = rn[9][1]
	epscpref = rn[10][0]
	epsclpref = rn[10][1]
	string wnsfx = ""
	if(stringmatch(wn,minilpref))			//mepsc
		wave/t mnname = $"mnname"
		FindValue/txop=2/text=wn mnname
		i = v_value
		if(i>-1)
			deletepoints i,1, mnname,$"mntime"
		endif
	endif
	if(stringmatch(wn,epsclpref))			//eepsc
		variable vp,ti

		vp = strsearch(wn,"_",0)
		if(vp>-1)
			wnsfx = wn[strlen(epscpref),vp-1]
			i = str2num(wn[vp+1,inf])
		endif
	endif
	if(i>-1)
		if(waveexists(root:$(fd):$"propertyinfo")==0)			//make register wave if not exist
			make/n=(0,9) root:$(fd):$"propertyinfo"
		endif
		wave pw = root:$(fd):$"propertyinfo"
		n = numpnts(pw)/9
		insertpoints n,1,pw
		pw[n][0] = m
		for(j=1;j<9;j+=1)
			if(waveexists($rn[j][0]+wnsfx))
				wave prop = $rn[j][0]+wnsfx
				pw[n][j] = prop[i]
				deletepoints i,1,prop
			endif
		endfor
	endif
	return ERROR_SUCCESS
End
Override Function Help(fnstr)
											//shows help context.
											//call: GetRecentPath
	string fnstr							//funcNameStr
											//is the name of the function which the user need help.
											//If the fnstr fails to hit the available function names, a dialog
											//_will be present for user to choose.
	string winn
//	if(strlen(winlist("!Procedure",";",""))==0)
//		print "No precedure found."
//		return ERROR_INVALID_PROCEDURE
//	endif
//	string pl = winlist("!Procedure",";","WIN:128")
	variable i,n

	string pn, fl
	fl = ""
	variable fid

	fl = functionlist("*",";","Kind:2")//WIN:"+pn)
	fl = removefromlist(greplist(fl,"(?i)((_back\d*|hook|proc)$|test|history)"),fl)

	fid = WhichListItem(fnstr,fl,";",0,0)
//	print fid,itemsinlist(fl)
	if((fid<0)||(fid>itemsinlist(fl)-1))
		prompt fid,"Choose Function:",popup,fl
		doprompt "Help Context",fid
		if(v_flag ==1)
			return ERROR_USER_CANCELED
		else
			help(stringfromlist(fid-1,fl))
			return ERROR_SUCCESS
		endif
	endif

	fnstr = stringfromlist(fid,fl,";")
	displayhelptopic/z fnstr
	if(v_flag)
		//		DisplayProcedure/w=$pn fnstr
		string path = getrecentpath(7,FALSE)
		string hfn = parsefilepath(3,functionpath(getrtstackinfo(1)),":",0,0)+".ihf"
//		print hfn
		variable ref
		open/r/z ref as path+"Igor Help Files:"+hfn
		if(ref)
			close ref
			ref = 0
			execute/p/q/z "LOADFILE \""+path+"Igor Help Files:"+hfn+"\""
			execute/p/q/z "help(\""+fnstr+"\")"
		else
			open/r/z ref as path+hfn
			if(ref)
				close ref
				ref = 0
				execute/p/q/z "LOADFILE \""+path+hfn+"\""
				execute/p/q/z "help(\""+fnstr+"\")"
			else
		//		print "No help file has found."
				DoAlert/t="Error",0,"\rCould not find the help file which contains \r\rthe topic \""+fnstr+"\"."
				return ERROR_INVALID_PROCEDURE
			endif
		endif
		//	killwave("funn",FALSE,TRUE)
	endif
	return ERROR_SUCCESS
End
Override Function RecHistory()
											//records procedure behaviours to log.
//	string ps								//parameters
											//call: KillWave
	variable i,n
	string pl,arg
	if(waveexists($"exhis")==0)
		make/t/n=(0,2) $"exhis"
	endif
	wave/t exhis

	string func = GetRTStackInfo(2)
	string finfo = functioninfo(func)
	n = NumberByKey("N_PARAMS", functioninfo(func))

	pl = ""
	variable argt
	if(n)
		if(waveexists($"tmpexhis")==0)
			return ERROR_INVALID_WAVE
		endif
		wave/t tmpexhis
		if(numpnts(tmpexhis)!=n)
			return ERROR_INDEX_OUT_OF_RANGE
		endif
		for(i=0;i<n;i+=1)
			arg = tmpexhis[i]
			argt = NumberByKey("PARAM_"+num2str(i)+"_TYPE", functioninfo(func))
			switch(argt)
				case 1:
				case 2:
				case 4:
				case 8:
				case 16:
				case 32:
				case 64:
					break
				case 8192:
					arg = "\""+arg+"\""
					break
			endswitch
			pl = pl + arg + ","
		endfor
		if(strlen(pl))
			pl = pl[0,strlen(pl)-2]
		endif
	endif
	pl = "("+pl+")"
	string cmd,et

	cmd = func + pl
	et = Secs2Date(DateTime,-2,"/")+" "+time()
//	print func
	n = dimsize(exhis,0)
	cmd = replacestring("\\",cmd,"\\\\")
	insertpoints n,1,exhis
	exhis[n][0]=cmd
	exhis[n][1]=et
	killwave("tmpexhis",TRUE,TRUE)
//	print cmd
	return ERROR_SUCCESS
End
Override Function/S GetRecentPath(ptype,stype)
								//returns the string of the path which has been
								//_latest used.
								//call: KillWave
	variable ptype				//pathType
								//is the type of path to be checked.
	variable stype				//separatorType
								//is used to decide which separator character will be
								//_used in the output path string.
	string path = ""
	string pl,pstr
	pl = pathlist("!Igor",";","")

	switch (ptype)
		case ALL:
			if(WhichListItem("recent",pl,";",0,0)!=-1)
				pathinfo $"recent"
				if(strlen(s_path))
					path = s_path
					if(stype)
						path = parsefilepath(5,path,"\\",0,0)
					endif
					return path
				endif
			endif
		case 0:					//home
			pstr = "home"
			if(whichlistitem(pstr,pl,";",0,0)!=-1)
				pathinfo $pstr
				path = s_path
			endif
			//			print "0",path
			if(strlen(path))
				break
			endif
			if(ptype!=ALL)
				break
			endif
		case 1:					//recent
			pstr = "recent"
			if(whichlistitem(pstr,pl,";",0,0)!=-1)
				pathinfo $pstr
				path = s_path
			else
				if(waveexists($"def_recent"))
					wave/t def_recent
					path = def_recent[0]
				else
					variable ref
					string rpstr = specialdirpath("Igor Pro User Files",0,0,0)+"recentfile"
					open/z/r ref as rpstr
					if(ref)
						close ref
						ref = 0
					endif
					if(!v_flag)
						loadwave/o/q/t rpstr

						if(waveexists($"def_recentfile"))
							wave/t def_recentfile
							if(dimsize(def_recentfile,0))
								path = def_recentfile[0][1]
							endif
						endif
					endif
				endif
				KillWave("def_recentfile",TRUE,TRUE)
			endif
//			print "1",path
			if(strlen(path))
				break
			endif
			if(ptype!=ALL)
				break
			endif
		case 2:					//fn
			svar lfn = $"fn"
			if(svar_exists(lfn))
				path = parsefilepath(1, parsefilepath(5,lfn,":",0,0),":",1,0)
			endif
//			print "2",path
			if(strlen(path))
				break
			endif
			if(ptype!=ALL)
				break
			endif
		case 3:					//all other non-igor path
			variable i,n
			n = itemsinlist(pl)
			for(i=0;i<n;i+=1)
				pstr = stringfromlist(i,pl)
				pathinfo $pstr
				path = s_path
				if(strlen(path))
					break
				endif
			endfor
//			print "3",path
			if(strlen(path))
				break
			endif
			if(ptype!=ALL)
				break
			endif
		case 4:					//document
			path = SpecialDirPath("Documents", 0, 0, 0)
			if(strlen(path))
				break
			endif
			if(ptype!=ALL)
				break
			endif
		case 5:					//desktop
			path = SpecialDirPath("Desktop", 0, 0, 0)
			if(strlen(path))
				break
			endif
			if(ptype!=ALL)
				break
			endif
//		case 6:					//procedure
//			path = parsefilepath(1,functionpath(getrtstackinfo(1)),":",1,0)
//			break
		case 7:
			path = specialdirpath("Igor Pro User Files",0,0,0)
			if(strlen(path))
				break
			endif
			if(ptype!=ALL)
				break
			endif
		default:
	endswitch
	if(ptype!=7)
		newpath/o/z/q recent,path
	endif
	if(stype)
		path = parsefilepath(5,path,"\\",0,0)
	endif
	return path
End
Override Function SaveRecentPath(fstr)
												//saves the path and the name of recently operated file.
												//call: KillWave, GetRecentPath
	string fstr
	string pstr = ""
	if(strlen(fstr)==0)
		return ERROR_INVALID_FILE
	endif
	string path = GetRecentPath(7,FALSE)
	variable ref
	fstr = parsefilepath(5,fstr,":",0,0)
	string et = Secs2Date(DateTime,-2,"/")+" "+time()
	string sfx = "_"+Secs2Date(DateTime,-2,"_")
	if(cmpstr(pstr[strlen(pstr)-1],":")==0)		//a dir
		pstr = fstr
	else											//a file
		pstr = parsefilepath(1,fstr,":",1,0)
	endif
	open/z/r ref as path+"recentfile"
	if(ref)
		close ref
		ref = 0
	endif
	if(!v_flag)
		loadwave/o/q/t path+"recentfile"
	endif
	if(waveexists($"def_recentfile"))
		wave/t def_recentfile
		if(dimsize(def_recentfile,0)>500)
			duplicate/o def_recentfile,$"def_recentfile"+sfx
			save/o/t $"def_recentfile"+sfx as path+"def_recentfile"+sfx
			redimension/n=(0,3) def_recentfile
		endif
		insertpoints 0,1,def_recentfile
	else
		make/o/n=(1,3)/t def_recentfile
	endif

	fstr = parsefilepath(5,fstr,"\\",0,0)
	def_recentfile[0][0] = fstr
	def_recentfile[0][1] = pstr
	def_recentfile[0][2] = et
	save/o/t def_recentfile as path+"recentfile"



	KillWave("def_recentfile;def_recentfile"+sfx+";",TRUE,TRUE)
//	print pstr
	return ERROR_SUCCESS
End
Override Function/S GetErrMsg(errcode)
												//returns the message string of given error code.
	variable errcode							//errorCode
												//is the number of error code defined by the procedure.
	string errmsg
	switch (errcode)
		case 0:
			errmsg = "ERROR_SUCCESS"
			break
		case 1:
			errmsg = "ERROR_NOT_INITIALIZED"
			break
		case 2:
			errmsg = "ERROR_NOT_NEEDED"
			break
		case 3:
			errmsg = "ERROR_NOT_ALLOWED"
			break
		case 4:
			errmsg = "ERROR_IGNORED"
			break
		case 5:
			errmsg = "ERROR_DISABLED"
			break
		case 6:
			errmsg = "ERROR_INDEX_OUT_OF_RANGE"
			break
		case 7:
			errmsg = "ERROR_QUICK_MODE:"
			break
		case 101:
			errmsg = "ERROR_USER_CANCELED"
			break
		case 102:
			errmsg = "ERROR_USER_ABORTED"
			break
		case 103:
			errmsg = "ERROR_ZERO_OBJECT_LENGTH"
			break
		case 201:
			errmsg = "ERROR_INVALID_WAVE"
			break
		case 202:
			errmsg = "ERROR_INVALID_RES_WAVE"
			break
		case 203:
			errmsg = "ERROR_WAVE_NOT_KILLED"
			break
		case 204:
			errmsg = "ERROR_NO_TARGET_WAVE"
			break
		case 301:
			errmsg = "ERROR_INVALID_WINDOW"
			break
		case 302:
			errmsg = "ERROR_WINDOW_NOT_KILLED:"
			break
		case 401:
			errmsg = "ERROR_INVALID_FILE"
			break
		case 501:
			errmsg = "ERROR_INVALID_CONTROL"
			break
		case 502:
			errmsg = "ERROR_INVALID_CTRL_TEXT"
			break
		case 601:
			errmsg = "ERROR_INVALID_FOLDER"
			break
		case 801:
			errmsg = "ERROR_INVALID_PROCEDURE"
			break
		case 802:
			errmsg = "ERROR_TOO_OLD_VERSION"
			break
		case 1001:
			errmsg = "ERROR_MINI_TOO_NARROW"
			break
		case 1002:
			errmsg = "ERROR_MINI_TOO_BIG"
			break
		case 1003:
			errmsg = "ERROR_MINI_NOT_BACK"
			break
		case 1004:
			errmsg = "ERROR_MINI_DXPFIT_ERR"
			break
		case 1005:
			errmsg = "ERROR_MINI_LINFIT_ERR"
			break
		case 1006:
			errmsg = "ERROR_MINI_LIKE_LINE"
			break
		case 1007:
			errmsg = "ERROR_MINI_AUTO_JUDGE"
			break
		case 1008:
			errmsg = "ERROR_MINI_SUCCESS"
			break
		default:
			errmsg = "ERROR_UNKNOWN_ERROR"
	endswitch
	errmsg = replacestring("_",errmsg[6,inf]," ")
	return errmsg
End
Override Function SetError(err)
									//saves error information during function and
									//_operation execution.
									//call: GetErrMsg
	variable err					//errorCode
	if(waveexists($"exerr")==0)
		make/n=(0,2)/t exerr		//is the wave which holds the error information during
									//_procedure execution.
									//While the user-defined "operations" can return the error
									//_code generated during execution, the functions have to
									//_return the accurate result values. In these cases, the
									//_errors will be saved in this wave.
									//It is a (n,2)-two-dimensional wave (matrix) where the
									//_columns are defined as:
									//  column 0:    Function name.
									//  column 1:    Error code message.
	endif
	wave/t exerr
	variable n = dimsize(exerr,0)
	insertpoints n,1,exerr
	exerr[n][0] = GetRTStackInfo(2)
	exerr[n][1] = GetErrMsg(err)
	return ERROR_SUCCESS
End
Override Function AfterCompiledHook()
									//is used to automatically start background checking process.
									//call: CheckUpdate
//print "mepsc hook"
	CheckUpdate()
//	print "yyy"
	return ERROR_SUCCESS
End
Override Function DisableProc()
				//call: KillWave
	string dastr = "Disable the running procedure \""
	dastr += parsefilepath(3,functionpath(getrtstackinfo(1)),":",0,0)

	dastr += "\" will remove the procedure from current experiment."
	dastr += "Do it now?"
	DoAlert/t="Disable Procedure" 1,dastr
	if(v_flag==1)
		string pfn = parsefilepath(0,functionpath(getrtstackinfo(1)),":",1,0)
		string path = parsefilepath(1,functionpath(getrtstackinfo(1)),":",1,0)
		make/o/n=1/t def_procname = pfn
		save/t/o def_procname as path+"procname"
		killwave("def_procname",TRUE,TRUE)
		copyfile/o/z path+pfn as path+"dproc"
		execute/p/q/z "closeproc/D=1/NAME=\""+pfn+"\""
//		execute "movefile/o/z \""+path+pfn+"\" as \""+path+"dproc\""
//		execute/p/q/z "sleep/q/s 2"
		execute/p/q/z "KillProc()"
		execute/p/q/z "COMPILEPROCEDURES "
	endif
	return ERROR_SUCCESS
End
Override Function KillProc()
				//call: KillWave
	string pn	//procname
	string path = specialdirpath("Igor Pro User Files",0,0,0)+"Igor Procedures:"
	variable ref
	open/z/r ref as path+"procname"
	if(ref)
		close ref
		ref = 0
	endif
	if(!v_flag)
		loadwave/o/q/t path+"procname"

		if(waveexists($"def_procname"))
			wave/t def_procname
			if(numpnts(def_procname))
				pn = def_procname[0]
				killwave("def_procname",TRUE,TRUE)
				if(strlen(pn))
					open/z/r ref as path+pn
					if(ref)
						close ref
						ref = 0
						deletefile/z path+pn
						return 0
					endif
				endif
			endif
		endif
	endif
	return ERROR_SUCCESS
End
