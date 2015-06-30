use test
db.posts.aggregate([
    {$match: {
        //{$or: {state: "CT", state: "NJ"}},
        state: "CT" 
        //"pop": {$gt: 100} 
    }},
    // {$group: {_id:null, avg_population: {$avg: "$pop"}}}
])
