use blog
db.posts.aggregate([
    {$project: {_id:1, comments:1}},
    {$unwind: "$comments"},
    {$group: {_id:"$comments.author", num_comments:{$sum:1}}},
    {$project: {_id:0, author:"$_id", num_comments:1}},
    {$sort: {num_comments: -1}}
    {$limit: 1}
])
