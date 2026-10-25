/*
 * C-Turkey calibration engine
 * A JavaScript port of rcarbon's standard (14C BP space) calibration:
 *   calibrate(x, errors, calCurves, normalised = TRUE, eps = 1e-5)
 *   hpdi(x, credMass)
 * Crema, E.R. & Bevan, A. (2021) Inference from large sets of radiocarbon
 * dates: software and methods. Radiocarbon 63(1): 23-39.
 */
(function (root) {
  "use strict";

  var EPS = 1e-5;
  var SQRT_2PI = Math.sqrt(2 * Math.PI);

  // Time ranges rcarbon uses for each curve (older curves stop at 50 ka).
  var TIME_RANGE = {
    intcal20: [55000, 0],
    shcal20: [55000, 0],
    marine20: [55000, 0],
    intcal13: [50000, 0]
  };

  /**
   * Interpolate a raw curve ({bp, c14, err}, bp descending) onto a 1-year grid,
   * as rcarbon does with stats::approx(). Returns typed arrays.
   */
  function prepareCurve(raw) {
    var bp = raw.bp, c14 = raw.c14, err = raw.err;
    var n = bp.length;
    var max = bp[0], min = bp[n - 1];
    var len = max - min + 1;
    var years = new Int32Array(len);
    var mu = new Float64Array(len);
    var tau2 = new Float64Array(len);
    var j = 0;
    for (var i = 0; i < len; i++) {
      var y = max - i;
      while (j < n - 2 && bp[j + 1] >= y) j++;
      var x0 = bp[j], x1 = bp[j + 1];
      var t = x0 === x1 ? 0 : (x0 - y) / (x0 - x1);
      if (y === x1) t = 1;
      var m = c14[j] + t * (c14[j + 1] - c14[j]);
      var e = err[j] + t * (err[j + 1] - err[j]);
      years[i] = y;
      mu[i] = m;
      tau2[i] = e * e;
    }
    var range = TIME_RANGE[raw.name] || [max, min];
    return { name: raw.name, raw: raw, years: years, mu: mu, tau2: tau2, timeRange: range };
  }

  /**
   * Calibrate one date. Returns {calBP: [...], PrDens: [...]} with calBP
   * descending and only non-zero densities kept, exactly like a rcarbon grid.
   * Throws if the age falls outside the curve.
   */
  function calibrate(age, error, curve, eps) {
    eps = eps == null ? EPS : eps;
    var len = curve.years.length;
    var dens = new Float64Array(len);
    var e2 = error * error;
    var sum = 0, i;
    for (i = 0; i < len; i++) {
      var v = e2 + curve.tau2[i];
      var sd = Math.sqrt(v);
      var z = (age - curve.mu[i]) / sd;
      var d = Math.exp(-0.5 * z * z) / (sd * SQRT_2PI);
      if (d < eps) d = 0;
      dens[i] = d;
      sum += d;
    }
    if (!(sum > 0)) {
      throw new RangeError("outside");
    }
    // normalise_densities(): divide, drop < eps, divide again
    var sum2 = 0;
    for (i = 0; i < len; i++) {
      var p = dens[i] / sum;
      if (p < eps) p = 0;
      dens[i] = p;
      sum2 += p;
    }
    var hi = curve.timeRange[0], lo = curve.timeRange[1];
    var calBP = [], PrDens = [];
    for (i = 0; i < len; i++) {
      var p2 = dens[i] / sum2;
      var yr = curve.years[i];
      if (p2 > 0 && yr <= hi && yr >= lo) {
        calBP.push(yr);
        PrDens.push(p2);
      }
    }
    if (!calBP.length) throw new RangeError("outside");
    return { calBP: calBP, PrDens: PrDens };
  }

  /**
   * Highest posterior density intervals, as rcarbon::hpdi().
   * Intervals are split wherever the calendar years are not contiguous.
   * Returns [{startCalBP, endCalBP, prob}], oldest first.
   */
  function hpdi(grid, credMass) {
    credMass = credMass == null ? 0.954 : credMass;
    var d = grid.PrDens, n = d.length, i;
    var total = 0;
    for (i = 0; i < n; i++) total += d[i];
    var sorted = d.slice().sort(function (a, b) { return b - a; });
    var cum = 0, height = sorted[n - 1];
    for (i = 0; i < n; i++) {
      cum += sorted[i];
      if (cum >= total * credMass) { height = sorted[i]; break; }
    }
    var out = [], cur = null;
    for (i = 0; i < n; i++) {
      var inside = d[i] >= height;
      var contiguous = cur && grid.calBP[i] === grid.calBP[i - 1] - 1 && d[i - 1] >= height;
      if (inside && contiguous) {
        cur.endCalBP = grid.calBP[i];
        cur.prob += d[i];
      } else if (inside) {
        cur = { startCalBP: grid.calBP[i], endCalBP: grid.calBP[i], prob: d[i] };
        out.push(cur);
      } else {
        cur = null;
      }
    }
    return out;
  }

  function weightedMean(grid) {
    var s = 0, w = 0;
    for (var i = 0; i < grid.calBP.length; i++) {
      s += grid.calBP[i] * grid.PrDens[i];
      w += grid.PrDens[i];
    }
    return s / w;
  }

  // R's round(): IEC 60559, half to even.
  function rround(x) {
    var r = Math.round(x);
    if (Math.abs(x % 1) === 0.5 && r % 2 !== 0) r -= 1;
    return r;
  }

  // 1950 - BP, labelled the same way as the original Shiny app.
  function fmtYear(bp) {
    var y = 1950 - bp;
    if (y <= 0) return Math.abs(rround(y)) + " BCE";
    return rround(y) + " CE";
  }

  var api = {
    prepareCurve: prepareCurve,
    calibrate: calibrate,
    hpdi: hpdi,
    weightedMean: weightedMean,
    fmtYear: fmtYear,
    rround: rround,
    TIME_RANGE: TIME_RANGE
  };

  if (typeof module !== "undefined" && module.exports) module.exports = api;
  else root.C14 = api;
})(this);
