use students
db.grades.findAndModify({
    query: { type: "homework" },
    sort: { score: 1 },
    remove: true
})
