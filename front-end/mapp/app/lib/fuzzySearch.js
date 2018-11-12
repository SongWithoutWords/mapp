function levenshteinDistance(source, target) {
    const n = source.length;
    const m = target.length;
  
    if (n === 0) {
      return m;
    }
  
    if (m === 0) {
      return n;
    }
  
    // distance matrix
    var d = [];
  
    //initialize first column to 0...m
    // d[row][column]
    for (var i = 0; i <= m; i++) {
      d[i] = [];
      d[i][0] = i;
    }
  
    //initialize first row to 0...n
    for (var j = 0; j <= n; j++) {
      d[0][j] = j;
    }
  
    // set cell values
    var cost = 0;
    for (var j = 1; j <= n; j++) {
      for (var i = 1; i <= m; i++) {
        cost = (source.charAt(j - 1) === target.charAt(i - 1)) ? 0: 1;
        d[i][j] = Math.min(
          d[i - 1][j] + 1,
          d[i][j - 1] + 1,
          d[i - 1][j - 1] + cost
        );
      }
    }
  
    // Return the levenshteinDistance
    return d[m][n];
  }
  
//   let source = "GUMBO";
//   let target = "GAMBOL";
//   console.log(levenshteinDistance(source, target));