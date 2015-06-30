use enron
db.messages.aggregate([
    {$project: {_id: 1, headers: 1}},    
    {$unwind: '$headers.To'},
    {$group: {_id: '$_id', 'headersFrom': {$addToSet: '$headers.From'}, 'headersTo': {$addToSet: '$headers.To'}}},
    {$unwind: '$headersFrom'},
    {$unwind: '$headersTo'},
    {$group: {_id: { from: '$headersFrom', to: '$headersTo'}, 'count': {$sum: 1}}},
    {$sort: {count: -1} }
])
