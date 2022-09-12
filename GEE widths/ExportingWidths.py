##Author: Ryan Riggs
##Date: 9/12/2022
##Set up GEE variables
import ee 
ee.Initialize()
sword = ee.FeatureCollection('users/rriggs/AllxSections3')
records = ee.FeatureCollection('users/rriggs/grdc_gee1')
grdc = ee.FeatureCollection('users/rriggs/RC/validQ_1984')#'users/rriggs/RC/LocationsGaugesGRWLfiltV3')
grwl_cline = ee.FeatureCollection('users/eeProject/GRWL_summaryStats')
grwl_cline = ee.FeatureCollection('users/eeProject/GRWL_summaryStats')
##List of gauges. Only 3 shown here. 
p = ['05BN009_HYDAT',
'3141310_GRDC',
'5654310_GRDC']

##Functions. 
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



