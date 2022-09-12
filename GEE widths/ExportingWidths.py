import ee 
ee.Initialize()
#from ee_plugin import Map 

sword = ee.FeatureCollection('users/rriggs/AllxSections3')
records = ee.FeatureCollection('users/rriggs/grdc_gee1')
#grdc = ee.FeatureCollection('users/rriggs/GSIM_plus_India')
grdc = ee.FeatureCollection('users/rriggs/RC/validQ_1984')#'users/rriggs/RC/LocationsGaugesGRWLfiltV3')
grwl_cline = ee.FeatureCollection('users/eeProject/GRWL_summaryStats')
# def extraFunction(ryan):
#x = require('users/eeProject/RivWidthCloudPaper:functions_Landsat578/functions_waterClassification_Jones2019.js')
# fls = require('users/eeProject/RivWidthCloudPaper:functions_Landsat578/functions_landsat.js')
# flsh = require('users/eeProject/RivWidthCloudPaper:rwc_landsat.js')
# fnsLandsat = require('users/eeProject/RivWidthCloudPaper:functions_Landsat578/functions_landsat.js')
# lsFun = require('users/eeProject/RivWidthCloudPaper:functions_Landsat578/functions_landsat.js')
# riverFun = require('users/eeProject/RivWidthCloudPaper:functions_river.js')
grwl_cline = ee.FeatureCollection('users/eeProject/GRWL_summaryStats')

def maximum_no_of_tasks(MaxNActive, waitingPeriod):
	"""maintain a maximum number of active tasks
	"""
	import time
	import ee
	ee.Initialize()

	time.sleep(10)
	## initialize submitting jobs
	ts = list(ee.batch.Task.list())

	NActive = 0
	for task in ts:
		if ('RUNNING' in str(task) or 'READY' in str(task)):
			NActive += 1
	## wait if the number of current active tasks reach the maximum number
	## defined in MaxNActive
	while (NActive >= MaxNActive):
		time.sleep(waitingPeriod) # if reach or over maximum no. of active tasks, wait for 2min and check again
		ts = list(ee.batch.Task.list())
		NActive = 0
		for task in ts:
			if ('RUNNING' in str(task) or 'READY' in str(task)):
				NActive += 1
	return()
	
	
p = ['05BN009_HYDAT',
'3141310_GRDC',
'5654310_GRDC',
'2903050_GRDC',
'P.23_RID',
'3206800_GRDC',
'6955610_GRDC',
'70146_arcticnet',
'6680802_GRDC',
'1397_arcticnet',
'3631210_GRDC',
'08ND022_HYDAT',
'08NE118_HYDAT',
'4355600_GRDC',
'4355520_GRDC',
'6201.1_BOM',
'3265815_GRDC',
'1134420_GRDC',
'10LA004_HYDAT',
'4208060_GRDC',
'6983355_GRDC',
'10GC005_HYDAT',
'4208347_GRDC',
'3471100_GRDC',
'1599110_GRDC',
'05OJ002_HYDAT',
'11805_arcticnet',
'2997200_GRDC',
'6975110_GRDC',
'02HL006_HYDAT',
'4363500_GRDC',
'2903429_GRDC',
'1134630_GRDC',
'2369905_GRDC',
'9431_arcticnet',
'6731790_GRDC',
'4874730_GRDC',
'08NJ162_HYDAT',
'4149411_GRDC',
'2909153_GRDC',
'2469058_GRDC',
'09AA010_HYDAT',
'4203755_GRDC',
'1427350_GRDC',
'08LB083_HYDAT',
'2964100_GRDC',
'A4260691_BOM',
'05AD040_HYDAT',
'05BM005_HYDAT',
'02JB010_HYDAT',
'02JB023_HYDAT',
'9419_arcticnet',
'71085_arcticnet',
'71139_arcticnet',
'2910606_GRDC',
'10BE012_HYDAT',
'4208285_GRDC',
'6642100_GRDC',
'6421100_GRDC',
'6444200_GRDC',
'3308600_GRDC',
'10GC002_HYDAT',
'4208346_GRDC',
'08MH091_HYDAT',
'08NJ129_HYDAT',
'02RH061_HYDAT',
'05DA002_HYDAT',
'1428500_GRDC',
'09528600_USGS',
'136010A_BOM',
'04KA002_HYDAT',
'2907400_GRDC',
'2998702_GRDC',
'3637152_GRDC',
'02OG046_HYDAT',
'3629000_GRDC',
'08NJ167_HYDAT',
'143030A_BOM',
'6442600_GRDC',
'2903600_GRDC',
'P.24A_RID',
'1734500_GRDC',
'2064_AFD',
'5226100_GRDC',
'10259_arcticnet',
'2998500_GRDC',
'1134730_GRDC',
'1134500_GRDC',
'4362120_GRDC',
'02RH029_HYDAT',
'02LH004_HYDAT',
'3003_arcticnet',
'7072_arcticnet',
'7102_arcticnet',
'3844460_GRDC',
'3087_arcticnet',
'09383100_USGS',
'4152101_GRDC',
'1834110_GRDC',
'3210_arcticnet',
'2903428_GRDC',
'10317_arcticnet',
'6574154_GRDC',
'2589260_GRDC',
'2903983_GRDC',
'136021A_BOM',
'08NJ172_HYDAT',
'1591401_GRDC',
'2179100_GRDC',
'1591001_GRDC',
'2186950_GRDC',
'809358_BOM',
'1547310_GRDC',
'6502110_GRDC',
'3617810_GRDC',
'1531600_GRDC',
'1531450_GRDC',
'3649430_GRDC',
'A4261085_BOM',
'1159530_GRDC',
'2998510_GRDC',
'2144_AFD',
'6574350_GRDC',
'58648001_ANA',
'3613010_GRDC',
'3652320_GRDC',
'09429060_USGS',
'301011281109090_MLIT',
'6545800_GRDC',
'02YK006_HYDAT',
'2260400_GRDC',
'6970500_GRDC',
'6977100_GRDC',
'1147010_GRDC',
'2912600_GRDC',
'03OD005_HYDAT',
'4244501_GRDC',
'2909150_GRDC',
'6123350_GRDC',
'6123310_GRDC',
'6212610_GRDC',
'2148_AFD',
'6221670_GRDC',
'6221501_GRDC',
'05PE005_HYDAT',
'4243275_GRDC',
'6603110_GRDC',
'4214035_GRDC',
'02OF020_HYDAT',
'3265600_GRDC',
'3265910_GRDC',
'10HC007_HYDAT',
'64843000_ANA',
'3627000_GRDC',
'3275800_GRDC',
'11147001_CCRR',
'5126_AFD',
'4237_AFD',
'6421500_GRDC',
'08LE112_HYDAT',
'6335075_GRDC',
'1749100_GRDC',
'6233652_GRDC',
'09423650_USGS',
'05GF002_HYDAT',
'9060_AFD',
'309181289913310_MLIT',
'10LB006_HYDAT',
'302111282216010_MLIT',
'302111282216060_MLIT',
'302111282216150_MLIT',
'10KA001_HYDAT',
'10KA007_HYDAT',
'08KG001_HYDAT',
'6232905_GRDC',
'1159546_GRDC',
'02YL011_HYDAT',
'1255100_GRDC',
'1291100_GRDC',
'6233223_GRDC',
'10KA006_HYDAT',
'6401601_GRDC',
'07OB004_HYDAT',
'15293200_USGS',
'15292800_USGS',
'05DF004_HYDAT',
'1656_In_IWRIS',
'4213930_GRDC',
'10KA008_HYDAT',
'10KA009_HYDAT',
'05DF009_HYDAT',
'302041282204390_MLIT',
'3269_AFD',
'2080_AFD',
'9175_AFD',
'5127_AFD',
'5128_AFD',
'4212_AFD',
'4165_AFD',
'1647_AFD',
'301081281107280_MLIT',
'303031283305540_MLIT',
'302041282204400_MLIT',
'302041282204460_MLIT',
'302041282204470_MLIT',
'65365000_ANA',
'08DB013_HYDAT',
'08MH098_HYDAT',
'6935535_GRDC',
'6854901_GRDC',
'6854273_GRDC',
'2179_In_IWRIS',
'6854112_GRDC',
'02GH011_HYDAT',
'1640_In_IWRIS',
'2080_In_IWRIS',
'2043_In_IWRIS',
'10311001_CCRR',
'07322007_CCRR',
'11143002_CCRR',
'306021286614210_MLIT',
'17710000_ANA',
'05CH007_HYDAT',
'4213021_GRDC',
'05EC005_HYDAT',
'4213860_GRDC',
'03LF002_HYDAT',
'05CC011_HYDAT',
'05BN002_HYDAT',
'1159547_GRDC',
'09404001_CCRR',
'304101284408090_MLIT',
'307091287712160_MLIT',
'307091287712200_MLIT',
'302091282209050_MLIT',
'02AB023_HYDAT',
'05BA002_HYDAT',
'301081281107290_MLIT',
'302011282218110_MLIT',
'304081284408060_MLIT',
'306051286611100_MLIT',
'306051286611110_MLIT',
'309081289911160_MLIT',
'309061289901360_MLIT',
'309061289901450_MLIT',
'306041286606440_MLIT',
'02KF012_HYDAT',
'3629001_GRDC',
'3618051_GRDC',
'3626000_GRDC',
'9990007_BOM',
'1634500_GRDC',
'05OC017_HYDAT',
'07355001_CCRR',
'11040001_CCRR',
'07322008_CCRR',
'07322006_CCRR',
'07322005_CCRR',
'07322002_CCRR',
'07321007_CCRR',
'3638050_GRDC',
'3620000_GRDC',
'3623100_GRDC',
'18850000_ANA',
'4149350_GRDC',
'02RH072_HYDAT',
'02LH032_HYDAT',
'02RF009_HYDAT',
'2002_In_IWRIS',
'05AH005_HYDAT',
'05BN006_HYDAT',
'4116480_GRDC',
'17050001_ANA',
'13150003_ANA',
'07CD004_HYDAT',
'07DA006_HYDAT',
'07DA007_HYDAT',
'07DA033_HYDAT',
'07DA040_HYDAT',
'07DA041_HYDAT',
'02LB036_HYDAT',
'10100000_ANA',
'14100000_ANA',
'54547000_ANA',
'15558200_ANA',
'407209_BOM',
'37610110_ANA',
'11500000_ANA',
'414702_BOM',
'207017_BOM',
'112006B_BOM',
'414714_BOM',
'409710_BOM',
'04290335_USGS',
'12505450_USGS',
'12508850_USGS',
'03182700_USGS',
'15564879_USGS',
'09529362_USGS',
'09529360_USGS',
'01613095_USGS',
'07019090_USGS',
'07019185_USGS',
'07229300_USGS',
'12184500_USGS',
'04087142_USGS',
'15283700_USGS',
'13341570_USGS',
'03335678_USGS',
'01017060_USGS',
'01077000_USGS',
'01094000_USGS',
'01446000_USGS',
'01349000_USGS',
'01473470_USGS',
'02197324_USGS',
'08157600_USGS',
'08157540_USGS',
'01139000_USGS',
'04069530_USGS',
'01183994_USGS',
'0214253361_USGS',
'11509340_USGS',
'11509200_USGS',
'11509105_USGS',
'11486990_USGS',
'03108010_USGS',
'03085213_USGS',
'04293900_USGS',
'03110830_USGS',
'04084927_USGS',
'04084911_USGS',
'04085068_USGS',
'040850684_USGS',
'04085108_USGS',
'04085109_USGS',
'05487520_USGS',
'14211820_USGS',
'14211818_USGS',
'14211814_USGS',
'01573660_USGS',
'01573710_USGS',
'01575598_USGS',
'14144805_USGS',
'14211898_USGS',
'07263300_USGS',
'07263570_USGS',
'12418000_USGS',
'03260100_USGS',
'03260050_USGS',
'03238785_USGS',
'02433500_USGS',
'06935770_USGS',
'06935997_USGS',
'13010450_USGS',
'05587480_USGS',
'05411400_USGS',
'08400000_USGS',
'04271815_USGS',
'01571490_USGS',
'11453000_USGS',
'09163492_USGS',
'07122060_USGS',
'13075981_USGS',
'13068501_USGS',
'13068500_USGS',
'03327507_USGS',
'04119160_USGS',
'04023150_USGS',
'03062400_USGS',
'01636462_USGS',
'07164600_USGS',
'07164650_USGS',
'03049807_USGS',
'01643590_USGS',
'12392155_USGS',
'05420100_USGS',
'05420300_USGS',
'03611900_USGS',
'07024175_USGS',
'12435500_USGS',
'05382240_USGS',
'05382242_USGS',
'05382241_USGS',
'05383075_USGS',
'06253000_USGS',
'15280999_USGS',
'05422640_USGS',
'05422650_USGS',
'05422600_USGS',
'05422470_USGS',
'01362090_USGS',
'01362182_USGS',
'01359133_USGS',
'01361000_USGS',
'01359750_USGS',
'09402300_USGS',
'09428511_USGS',
'09428510_USGS',
'09429210_USGS',
'09429220_USGS',
'09429225_USGS',
'09429230_USGS',
'09429200_USGS',
'13170910_USGS',
'04099850_USGS',
'01056480_USGS',
'01056505_USGS',
'04061000_USGS',
'05288705_USGS',
'12388200_USGS',
'06195600_USGS',
'06681500_USGS',
'06685000_USGS',
'06687000_USGS',
'06765990_USGS',
'06770175_USGS',
'06770195_USGS',
'06770240_USGS',
'04195830_USGS',
'07160810_USGS',
'01451650_USGS',
'411214079465802_USGS',
'03026250_USGS',
'02162093_USGS',
'02129590_USGS',
'01618100_USGS',
'03294570_USGS',
'03294550_USGS',
'03292555_USGS',
'03293530_USGS',
'03292475_USGS',
'03292480_USGS',
'04168660_USGS',
'06478522_USGS',
'03426470_USGS',
'03426460_USGS',
'12471724_USGS',
'12473502_USGS',
'12473880_USGS',
'13173500_USGS',
'131726449_USGS',
'05533400_USGS',
'06115270_USGS',
'06294950_USGS',
'06296003_USGS',
'06296100_USGS',
'06175540_USGS',
'06465310_USGS',
'06465440_USGS',
'01356190_USGS',
'01357499_USGS',
'0351706800_USGS',
'01537560_USGS',
'01537500_USGS',
'01537000_USGS',
'03491544_USGS',
'03491000_USGS',
'03491300_USGS',
'05342000_USGS',
'453046089342804_USGS',
'04292700_USGS',
'12319501_USGS',
'07380401_USGS',
'07380400_USGS',
'295124089542100_USGS',
'06340590_USGS',
'06340905_USGS',
'05399550_USGS',
'05389400_USGS',
'05389500_USGS',
'05355250_USGS',
'08064500_USGS',
'13157296_USGS',
'13157160_USGS',
'13157000_USGS',
'13155800_USGS',
'13155750_USGS',
'13108150_USGS',
'13095360_USGS',
'13093150_USGS',
'13091000_USGS',
'13090999_USGS',
'13090998_USGS',
'1313457010_USGS',
'13134570_USGS',
'05448000_USGS',
'05437695_USGS',
'05551545_USGS',
'05550300_USGS',
'05551030_USGS',
'05549850_USGS',
'06888925_USGS',
'06890900_USGS',
'06892513_USGS',
'093710009_USGS',
'14320934_USGS',
'14211315_USGS',
'14211550_USGS',
'03020000_USGS',
'03431700_USGS',
'03431599_USGS',
'03431490_USGS',
'03193778_USGS',
'13082320_USGS',
'13076200_USGS',
'02350080_USGS',
'07373250_USGS',
'07010208_USGS',
'07005000_USGS',
'07001985_USGS',
'021459367_USGS',
'03423152_USGS',
'12452990_USGS',
'05430446_USGS',
'05430403_USGS',
'09144200_USGS',
'02342070_USGS',
'02335910_USGS',
'02335912_USGS',
'02335790_USGS',
'02335757_USGS',
'411955088280601_USGS',
'06329200_USGS',
'01455290_USGS',
'01460880_USGS',
'08332050_USGS',
'083299375_USGS',
'08329936_USGS',
'08330800_USGS',
'08330600_USGS',
'08330775_USGS',
'08330200_USGS',
'08329900_USGS',
'13290190_USGS',
'03085049_USGS',
'01636690_USGS',
'07350020_USGS',
'06893620_USGS',
'4208150_GRDC',
'6246110_GRDC',
'6854100_GRDC',
'6242030_GRDC',
'6242130_GRDC',
'6242170_GRDC',
'5866230_GRDC',
'08NJ130_HYDAT',
'5867710_GRDC',
'6604350_GRDC',
'4214565_GRDC',
'4208740_GRDC',
'6559130_GRDC',
'08LB020_HYDAT',
'6340501_GRDC',
'4208720_GRDC',
'4208888_GRDC',
'4214263_GRDC',
'6335485_GRDC',
'4215330_GRDC',
'64790000_ANA',
'02YK002_HYDAT',
'06EB004_HYDAT',
'14420000_ANA',
'414712_BOM',
'06200000_USGS',
'14207500_USGS',
'01151500_USGS',
'01465798_USGS',
'08156800_USGS',
'03237280_USGS',
'05533000_USGS',
'409001_BOM',
'410107_BOM',
'12389500_USGS',
'01364500_USGS',
'06181000_USGS',
'02036500_USGS',
'01078000_USGS',
'01440000_USGS',
'08405150_USGS',
'01442500_USGS',
'01574000_USGS',
'14123500_USGS',
'13038000_USGS',
'01571500_USGS',
'13068495_USGS',
'02399200_USGS',
'09522500_USGS',
'09382000_USGS',
'01617800_USGS',
'05539000_USGS',
'12302055_USGS',
'01555500_USGS',
'06453600_USGS',
'02465493_USGS',
'13095500_USGS',
'05551200_USGS',
'05550500_USGS',
'02342500_USGS',
'01161000_USGS',
'01464000_USGS',
'4208452_GRDC',
'4214080_GRDC',
'4213820_GRDC',
'6729465_GRDC',
'01AK001_HYDAT',
'4231590_GRDC',
'10ED003_HYDAT',
'4208230_GRDC',
'4244750_GRDC',
'10BE009_HYDAT',
'4208288_GRDC',
'6729370_GRDC']

#p = ['1134200_grdc','1134650_grdc']
def exportation (gauge, buffer):
	id = gauge
	loc = grdc.filter(ee.Filter.eq('Sttn_Nm', id))
	#loc = grdc.filter(ee.Filter.eq('Sttn_Nm',id))
	filt_gauge = loc
	xsections = sword.filterBounds(loc.geometry().buffer(buffer))
	#record = records.filter(ee.Filter.eq('Sttn_Nm', id)).filter(ee.Filter.notNull(['value'])).filter(ee.Filter.gt('value', 0))

	#print('size', xsections.size())
	#Map.addLayer(xsections.limit(10))

	def Mndwi(image):
		return(image.normalizedDifference(['Green', 'Swir1']).rename('mndwi'))

	def Mbsrv(image):
		return(image.select(['Green']).add(image.select(['Red'])).rename('mbsrv'))

	def Mbsrn(image):
		return(image.select(['Nir']).add(image.select(['Swir1'])).rename('mbsrn'))

	def Ndvi(image):
		return(image.normalizedDifference(['Nir', 'Red']).rename('ndvi'))

	def Awesh(image):
		return(image.expression('Blue + 2.5 * Green + (-1.5) * mbsrn + (-0.25) * Swir2', {
		'Blue': image.select(['Blue']),
		'Green': image.select(['Green']),
		'mbsrn': Mbsrn(image).select(['mbsrn']),
		'Swir2': image.select(['Swir2'])}))

	def Dswe(i):
		mndwi = Mndwi(i)
		mbsrv = Mbsrv(i)
		mbsrn = Mbsrn(i)
		awesh = Awesh(i)
		swir1 = i.select(['Swir1'])
		nir = i.select(['Nir'])
		ndvi = Ndvi(i)
		blue = i.select(['Blue'])
		swir2 = i.select(['Swir2'])

		t1 = mndwi.gt(0.124)
		t2 = mbsrv.gt(mbsrn)
		t3 = awesh.gt(0)
		t4 = (mndwi.gt(-0.44)
		.And(swir1.lt(900))
		.And(nir.lt(1500))
		.And(ndvi.lt(0.7)))
		t5 = (mndwi.gt(-0.5)
		.And(blue.lt(1000))
		.And(swir1.lt(3000))
		.And(swir2.lt(1000))
		.And(nir.lt(2500)))

		t = t1.add(t2.multiply(10)).add(t3.multiply(100)).add(t4.multiply(1000)).add(t5.multiply(10000))

		noWater = (t.eq(0)
		.Or(t.eq(1))
		.Or(t.eq(10))
		.Or(t.eq(100))
		.Or(t.eq(1000)))
		hWater = (t.eq(1111)
		.Or(t.eq(10111))
		.Or(t.eq(11011))
		.Or(t.eq(11101))
		.Or(t.eq(11110))
		.Or(t.eq(11111)))
		mWater = (t.eq(111)
		.Or(t.eq(1011))
		.Or(t.eq(1101))
		.Or(t.eq(1110))
		.Or(t.eq(10011))
		.Or(t.eq(10101))
		.Or(t.eq(10110))
		.Or(t.eq(11001))
		.Or(t.eq(11010))
		.Or(t.eq(11100)))
		pWetland = t.eq(11000)
		lWater = (t.eq(11)
		.Or(t.eq(101))
		.Or(t.eq(110))
		.Or(t.eq(1001))
		.Or(t.eq(1010))
		.Or(t.eq(1100))
		.Or(t.eq(10000))
		.Or(t.eq(10001))
		.Or(t.eq(10010))
		.Or(t.eq(10100)))

		iDswe = (noWater.multiply(0)
		.add(hWater.multiply(1))
		.add(mWater.multiply(2))
		.add(pWetland.multiply(3))
		.add(lWater.multiply(4)))

		return(iDswe.rename(['dswe']))

	def ClassifyWaterJones2019(img):
		dswe = Dswe(img)
		waterMask = dswe.eq(1).Or(dswe.eq(2)).rename(['waterMask'])
		out = waterMask.copyProperties(img)
		return(out.set({'system:time_start':img.get('system:time_start')}))

	def Ndvi(image):
		# // calculate ndvi
		ndvi = image.normalizedDifference(['Nir', 'Red']).rename('ndvi')
		return ndvi

	def Evi(image):
		# calculate the enhanced vegetation index
		evi = image.expression('2.5 * (Nir - Red) / (1 + Nir + 6 * Red - 7.5 * Blue)', {
		'Nir': image.select(['Nir']),
		'Red': image.select(['Red']),
		'Blue': image.select(['Blue'])
		})
		return evi.rename(['evi'])

	def Mndwi(image):
		# calculate mndwi
		mndwi = image.normalizedDifference(['Green', 'Swir1']).rename('mndwi')
		return mndwi

	def ClassifyWaterZou2018(image):
		mndwi = Mndwi(image)
		ndvi = Ndvi(image)
		evi = Evi(image)
		water = (mndwi.gt(ndvi).Or(mndwi.gt(evi))).And(evi.lt(0.1)).rename(['waterMask'])
		out = water.copyProperties(image)
		return(out.set({'system:time_start':image.get('system:time_start')}))




	def ExtractChannel(image):
		# // extract the channel water bodies from the water mask, based on connectivity to the reference centerline.
		connectedToCl = (image.Not().cumulativeCost(
			source = ee.Image().toByte().paint(grwl_cline, 1).And(image), #// only use the centerline that overlaps with the water mask
			maxDistance = 4000,
			geodeticDistance = False).eq(0))

		channel = image.updateMask(connectedToCl).unmask(0).updateMask(image.gte(0)).rename(['channelMask'])
		return(channel)



	def yrFun(f):
	  a = f.get('variable')
	  return(f.set({'Date':a}))

	#record = record.map(yrFun)

	# Match common names to the sensor-specific bands:
	LT5_BANDS = ['B1',   'B2',    'B3',  'B4',  'B5',    'B7',    'B6', 'pixel_qa']
	LE7_BANDS = ['B1',   'B2',    'B3',  'B4',  'B5',    'B7',    'B6', 'pixel_qa']
	LC8_BANDS = ['B2',   'B3',    'B4',  'B5',  'B6',    'B7',    'B10', 'pixel_qa']
	STD_NAMES = ['Blue', 'Green', 'Red', 'Nir', 'Swir1', 'Swir2', 'Temp', 'BQA']

	# load Landsat 5,7,8 collections:
	# TODO(GHA): combine 5, 7, and 8 collections:
	LT5 = ee.ImageCollection('LANDSAT/LT5_L1T_SR') \
		.select(LT5_BANDS, STD_NAMES)
	LT5 = ee.ImageCollection('LANDSAT/LT05/C01/T1_SR') \
		.select(LT5_BANDS, STD_NAMES)
	LE7 = ee.ImageCollection('LANDSAT/LE07/C01/T1_SR') \
	.select(LE7_BANDS, STD_NAMES).filterDate('1999-01-01', '2003-05-30')
	LC8 = ee.ImageCollection('LANDSAT/LC08/C01/T1_SR') \
	   .select(LC8_BANDS, STD_NAMES)
	bn = ['B2', 'B3', 'B4', 'B8','B11','B12','QA10','SCL']
	collection = LT5.merge(LE7).merge(LC8)
	collection = collection.filterBounds(xsections)
	collection = collection.filterDate('1984-01-01','2021-12-31')

	#
	sent = ee.ImageCollection("COPERNICUS/S2_SR").filterBounds(xsections)
	sent = sent.select(bn, STD_NAMES)

	def func_yoc(f):
	  a = f.get('system:time_start')
	  b = ee.Date(a).format("yyyy-MM-dd")
	  return(f.set({'system:id': b, 'ID':b, 'area': 100}))

	sent = sent.map(func_yoc)





	#


	def func_zwp(f):
	  a = f.get('system:time_start')
	  b = ee.Date(a).format("yyyy-MM-dd")
	  return(f.set({'system:id': b, 'ID':b, 'area': 900}))

	collection = collection.map(func_zwp)


	collection = collection.merge(sent)
	# #record = record.filter(ee.Filter.gte('value', 0)).filter()
	# def img(f):
	  # a = f.get('Date')
	  # b = f.get('value')
	  # c = ee.Image.constant(b)
	  # return(ee.Image(c).set({'Date': a, 'Q':b,'system:id':ee.String(a), 'ID':ee.String(a)}))

	# #imgColl = record.map(img)
	# toyFilter = ee.Filter.equals(
	  # leftField= 'ID',
	  # rightField= 'ID'
	# )
	# # Define the join.
	# innerJoin = ee.Join.inner('primary', 'secondary')
	# # Apply the join.
	# toyJoin = innerJoin.apply(collection,imgColl, toyFilter)

	# def func_isq(f):
	  # a = f.get('secondary')
	  # a = ee.Image(a).get('Q')
	  # b = f.get('primary')
	  # c = ee.Image(b).set({'Q':a})
	  # return(c)

	# combined = toyJoin.map(func_isq)

	# def func_elp(f):
	  # a = ee.Image(f)
	  # b = a.clip(xsections.geometry())
	  # return(b)

	#filtered = combined.map(func_elp)


	#/Maybe keep??
	filtered = collection
	sent = filtered.filter(ee.Filter.eq('area', 100))
	#print(sent.first())
	filtered = filtered.filter(ee.Filter.eq('area', 900))



	def Unpack(bitBand, startingBit, bitWidth):
		# unpacking bit bands
		# see: https://groups.google.com/forum/#!starred/google-earth-engine-developers/iSV4LwzIW7A
		return (ee.Image(bitBand)
				.rightShift(startingBit)
				.bitwiseAnd(ee.Number(2).pow(ee.Number(bitWidth)).subtract(ee.Number(1)).int()))
	def UnpackAllSR(bitBand):
		# apply Unpack function for multiple pixel qualities
		bitInfoSR = {
		'Cloud': [5, 1],
		'CloudShadow': [3, 1],
		'SnowIce': [4, 1],
		'Water': [2, 1]
		}
		unpackedImage = ee.Image.cat([Unpack(bitBand, bitInfoSR[key][0], bitInfoSR[key][1]).rename([key]) for key in bitInfoSR])
		return unpackedImage
	def AddFmaskSR(image):
		# // add fmask as a separate band to the input image
		temp = UnpackAllSR(image.select(['BQA']))

		fmask = (temp.select(['Water']).rename(['fmask'])
		.where(temp.select(['SnowIce']), ee.Image(3))
		.where(temp.select(['CloudShadow']), ee.Image(2))
		.where(temp.select(['Cloud']), ee.Image(4))
		.mask(temp.select(['Cloud']).gte(0)))

		return image.addBands(fmask)

	flagged = ee.ImageCollection(filtered).map(AddFmaskSR)


	def sentinelCloud(f):
		temp = f.select('BQA').rename('fmask')
		water = temp.where(temp.gt(6).Or(temp.eq(3)), ee.Image(7))
		all = water.where(water.lte(6), ee.Image(1))
		fmask = all.where(all.eq(7), ee.Image(2))
		return f.addBands(fmask)
	
	sent = ee.ImageCollection(sent).map(sentinelCloud)
	flagged = flagged.merge(sent)

	xsections = ee.FeatureCollection(xsections)

	waterMask = flagged.map(ClassifyWaterZou2018)#(x.ClassifyWaterJones2019)

	filt = xsections
	#Determine connectivity of water pixels to grwl centerline and filter out any water pixels more than 4km away from centerline.
	def GetCenterline(clDataset, bound):
		# // filter the GRWL centerline based on area of interest
		cl = clDataset.filterBounds(bound)
		return(cl)
	#riverFun.GetCenterline(grwl_cline, xsections)
	riverMask = waterMask.map(ExtractChannel)
	toyFilter = ee.Filter.equals(
	  leftField= 'system:index',
	  rightField= 'system:index'
	)
	# Define the join.
	innerJoin = ee.Join.inner('primary', 'secondary')
	# Apply the join.
	toyJoin = innerJoin.apply(riverMask,flagged, toyFilter)
	
	def combination (f):
		a = f.get('secondary')
		a = ee.Image(a)
		b = f.get('primary')
		c = ee.Image(b).addBands(a)
		return(c)
		
	combined = toyJoin.map(combination)


	def line2pt (f):
		f = f.set({'lineGeometry': f.geometry()})
		l = f.geometry().coordinates()
		g = ee.Geometry.MultiPoint(l, 'EPSG:4326')
		return(f.setGeometry(g))

	toyFilter = ee.Filter.equals(
		  leftField= 'node_id',
		  rightField= 'node_id'
		)
	innerJoin = ee.Join.inner('primary', 'secondary');
	def jng(f):
		a = f.get('primary')
		b = f.get('secondary')
		c = ee.Feature(a).copyProperties(b)
		return(c.set({'cloud':c.get('mean')}))


	def jng1(f):
	    a = f.get('primary')
	    b = f.get('secondary')
	    c = ee.Feature(a).copyProperties(b)
	    return(c.set({'width':c.get('mean'), 'length':ee.Feature(b).geometry().length()}))







	def processing(f):
		time = f.get('system:time_start')
		cld = ee.Image(f).select('fmask').gt(1)
		min = cld
		min = min.reduceRegions(xsections,ee.Reducer.mean())
		flags = ee.Image(f).select('channelMask').reduceRegions(xsections.map(line2pt),ee.Reducer.max().combine(ee.Reducer.count(),"", True))
		wd = ee.Image(f).select('channelMask').eq(1).reduceRegions(xsections,ee.Reducer.mean())
		toyJoin = innerJoin.apply(flags,min, toyFilter);
		combined = toyJoin.map(jng)
		secondJoin = innerJoin.apply(combined, ee.FeatureCollection(wd), toyFilter).map(jng1)
		def tm (f):
			a = f.set({'system:time_start':time, 'Sttn_Nm':id})
			return(a)
		return(secondJoin.map(tm))
		

	outputs = ee.ImageCollection(combined).map(processing)
	##Filtering makes it slower. 
	outputs = outputs.flatten()#.filter(ee.Filter.notNull(['width', 'cloud'])).filter(ee.Filter.lte('cloud', .25)).filter(ee.Filter.eq('count', 2)).filter(ee.Filter.eq('max',0)).filter(ee.Filter.gt('width', 0))
	return(outputs.sort('mean',False))#).filter(ee.Filter.notNull(['width', 'cloud'])))

for i in range(len(p)):
	out = exportation(p[i],2000)
	task = (ee.batch.Export.table.toDrive(
	collection = out,
	description = 'widths_' + '_' + str(p[i]),
	folder = 'Remaining_xsections_V3',#'Remaining_xsection_widths_updated',
	fileNamePrefix = 'Gauge_' + '_' + str(p[i]),
	fileFormat = 'csv'
	))

	task.start()
	#print(output.first())
	print('task', p[i], 'has started')
	maximum_no_of_tasks(10,90)



