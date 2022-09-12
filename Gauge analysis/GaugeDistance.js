var points = ee.FeatureCollection("users/ryanriggs7/allUpdated_endYear")
var filt = points.filter(ee.Filter.notNull(['year']))
var points = filt
Map.addLayer(points, {}, 'points')

// Max search distance is 5 km.
var spatialFilter = ee.Filter.withinDistance({
  distance: 110,
  leftField: '.geo',
  rightField: '.geo',
  maxError: 10
})

// Join the points to themselves.
var joined = ee.Join.saveAll({
  matchesKey: 'neighbors', 
  measureKey: 'distance',
  ordering: 'distance'
}).apply({
  primary: points, 
  secondary: points, 
  condition: spatialFilter
});

// Get rid of points w/o neighbors.
// Increase distance in the spatialFilter if
// you want them all to have neighbors.
var hasNearest = joined.map(function(f) {
  var neighsSize = ee.List(f.get('neighbors')).size();
  return f.set('neighsSize', neighsSize);
}).filter(ee.Filter.gt('neighsSize', 1));
//Map.addLayer(hasNearest, {color: 'red'}, 'hasNearest');
print(hasNearest.limit(10))

// Get distance to nearest point.
var withNearestDist = hasNearest.map(function(f) {
  var nearestDist = ee.Feature(ee.List(f.get('neighbors')).get(1))
      .get('distance');
  return f.set({'nearestDist': nearestDist});
});
//Map.addLayer(withNearestDist, {color: 'blue'}, 'withNearestDist');
print(withNearestDist.limit(10))


var distFun = function(f){
  var a = ee.FeatureCollection(ee.List(f.get('neighbors'))).map(function(x){
    var b = ee.Feature(x).set({'Redundant_Sttn_Nm': f.get('Sttn_Nm')})
    return(b)
  })
  return(a)
}

var processedDistances = withNearestDist.map(distFun)
Export.table.toDrive(processedDistances.flatten())
