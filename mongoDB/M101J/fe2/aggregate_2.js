use enron
db.messages.aggregate([
    {$project: {_id: 1, headers: 1}},    
    {$unwind: '$headers.To'},
    {$group: {_id: { from: '$headers.From', to: '$headers.To'}, 'count': {$sum: 1}}},
    {$sort: {count: -1} }
])
