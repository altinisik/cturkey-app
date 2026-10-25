/*
 * C-Turkey calibration figure
 * Draws the same figure as plot_custom_cal() in the original Shiny app,
 * as a standalone SVG string. Geometry follows R's base graphics:
 * an 8 x 6 inch device, 1 unit = 1/100 inch, par(mar = c(6, 5, 5, 6)),
 * 12 pt base font, 4% axis padding (xaxs = yaxs = "r").
 */
(function (root) {
  "use strict";

  var W = 800, H = 600;
  var PT = 100 / 72;                 // units per point
  var LINE = 20;                     // one margin line = 0.2 in
  var BASE = 12 * PT;                // 12 pt
  var LWD = 100 / 96;                // lwd = 1
  var MAR = { b: 6, l: 5, t: 5, r: 6 };
  var L = MAR.l * LINE, R = W - MAR.r * LINE, T = MAR.t * LINE, B = H - MAR.b * LINE;
  var FONT = "PlotSans, Arial, Helvetica, sans-serif";

  var PALETTES = {
    color: { main: "#666666", uncal: "#b6cbdb", curve: "#e67e22", curveShd: "#e67e22", curveShdOpacity: 0.2, marker: "#c0392b", text: "#34495e" },
    bw: { main: "#666666", uncal: "#d9d9d9", curve: "#000000", curveShd: "#e5e5e5", curveShdOpacity: 1, marker: "#000000", text: "#000000" }
  };

  // ---- R's axis tick algorithm (GEPretty: R_pretty with h = .8, h5 = 1.7) --
  function pretty(lo, hi, n) {
    n = n || 5;
    var h = 0.8, h5 = 1.7;
    var dx = hi - lo;
    var cell, unit, U;
    if (dx === 0 && hi === 0) { cell = 1; }
    else {
      cell = Math.max(Math.abs(lo), Math.abs(hi));
      var Uq = 1 + (h5 >= 1.5 * h + 0.5 ? 1 / (1 + h) : 1.5 / (1 + h5));
      Uq *= Math.max(1, n) * 2.220446e-16;
      if (dx < cell * Uq * 3) { cell = cell > 10 ? 9 + cell / 10 : cell; }
      else { cell = dx / n; }
    }
    U = Math.pow(10, Math.floor(Math.log10(cell)));
    unit = U;
    var ns;
    if ((ns = 2 * U) - cell < h * (cell - unit)) {
      unit = ns;
      if ((ns = 5 * U) - cell < h5 * (cell - unit)) {
        unit = ns;
        if ((ns = 10 * U) - cell < h * (cell - unit)) unit = ns;
      }
    }
    var a = Math.floor(lo / unit + 1e-7), b = Math.ceil(hi / unit - 1e-7);
    var out = [];
    for (var k = a; k <= b; k++) out.push(+(k * unit).toPrecision(12));
    return out;
  }

  // R formats a tick vector with a common number of decimals.
  function formatTicks(v) {
    var dec = 0;
    v.forEach(function (x) {
      var s = String(+x.toPrecision(10));
      var m = s.indexOf("e") >= 0 ? null : s.split(".")[1];
      if (m) dec = Math.max(dec, m.length);
    });
    return v.map(function (x) { return x.toFixed(dec); });
  }

  // R's axis() skips a label that would overlap the previous one drawn.
  function keepLabels(pos, extent, gap) {
    var keep = [], lastEdge = -Infinity;
    var order = pos.map(function (p, i) { return i; }).sort(function (a, b) { return pos[a] - pos[b]; });
    order.forEach(function (i) {
      var lo = pos[i] - extent[i] / 2, hi = pos[i] + extent[i] / 2;
      if (lo >= lastEdge + gap) { keep[i] = true; lastEdge = hi; } else keep[i] = false;
    });
    return keep;
  }

  function esc(s) {
    return String(s).replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;").replace(/"/g, "&quot;");
  }
  function f(x) { return (Math.round(x * 100) / 100).toString(); }

  function padRange(a, b) {      // xaxs = "r": extend by 4% each side
    var d = (b - a) * 0.04;
    return [a - d, b + d];
  }

  function text(x, y, s, o) {
    o = o || {};
    var attrs = 'x="' + f(x) + '" y="' + f(y) + '" font-size="' + f(o.size || BASE) + '"';
    if (o.anchor) attrs += ' text-anchor="' + o.anchor + '"';
    if (o.bold) attrs += ' font-weight="bold"';
    attrs += ' fill="' + (o.fill || "#000000") + '"';
    if (o.rotate) attrs += ' transform="rotate(' + o.rotate + " " + f(x) + " " + f(y) + ')"';
    return "<text " + attrs + ">" + esc(s) + "</text>";
  }

  function polyPoints(xs, ys) {
    var p = [];
    for (var i = 0; i < xs.length; i++) p.push(f(xs[i]) + "," + f(ys[i]));
    return p.join(" ");
  }

  /**
   * opts: { grid, hpd, mean, cra, error, id, calendar: "BCAD"|"BP", bw,
   *         curve (prepared), measure(text, size, bold) -> width,
   *         fmtYear, fontFaceCss (optional, embedded for export) }
   */
  function render(opts) {
    var pal = opts.bw ? PALETTES.bw : PALETTES.color;
    var bcad = opts.calendar === "BCAD";
    var grid = opts.grid;
    var measure = opts.measure;
    var cexAxis = 1.1, cexLab = 1.1, cexLabel = 1.0, cexMain = 1.3;
    var sAxis = BASE * cexAxis, sLab = BASE * cexLab, sLegend = BASE * cexLabel, sMain = BASE * cexMain;

    var toX = function (bp) { return bcad ? 1950 - bp : bp; };

    // Calibrated density polygon in calendar space
    var n = grid.calBP.length;
    var px = [], py = [];
    for (var i = 0; i < n; i++) { px.push(toX(grid.calBP[i])); py.push(grid.PrDens[i]); }
    var xvals = [px[0]].concat(px, [px[n - 1], px[0]]);
    var yvals = [0].concat(py, [0, 0]);
    var lo = Infinity, hi = -Infinity, ymax = 0;
    for (i = 0; i < xvals.length; i++) {
      if (yvals[i] > 1e-6) { lo = Math.min(lo, xvals[i]); hi = Math.max(hi, xvals[i]); }
      ymax = Math.max(ymax, yvals[i]);
    }
    // xlim in display order (left, right); BP runs older-to-younger
    var xlim = bcad ? [lo - 50, hi + 50] : [hi + 50, lo - 50];
    var ux = padRange(xlim[0], xlim[1]);
    var uy = padRange(0, ymax * 2.5);
    var X = function (x) { return L + (x - ux[0]) / (ux[1] - ux[0]) * (R - L); };
    var Y = function (y) { return B - (y - uy[0]) / (uy[1] - uy[0]) * (B - T); };

    var cra = opts.cra, err = opts.error;
    var uy2 = padRange(cra - 8 * err, cra + 8 * err);
    var Y2 = function (y) { return B - (y - uy2[0]) / (uy2[1] - uy2[0]) * (B - T); };

    var out = [];
    out.push('<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 ' + W + " " + H + '" width="' + W + '" height="' + H + '" font-family="' + FONT + '">');
    out.push("<defs>" + (opts.fontFaceCss ? "<style>" + opts.fontFaceCss + "</style>" : "") +
      '<clipPath id="plotRegion"><rect x="' + f(L) + '" y="' + f(T) + '" width="' + f(R - L) + '" height="' + f(B - T) + '"/></clipPath></defs>');
    out.push('<rect x="0" y="0" width="' + W + '" height="' + H + '" fill="#ffffff"/>');

    // Title
    var title = opts.id + " (" + cra + " \u00B1 " + err + " BP)";
    out.push(text((L + R) / 2, T - 2.05 * LINE, title, { size: sMain, bold: true, anchor: "middle", fill: pal.text }));

    // ---- x axis ----
    var xmin = Math.min(ux[0], ux[1]), xmaxv = Math.max(ux[0], ux[1]);
    var span = Math.abs(xlim[1] - xlim[0]);
    var step = span > 500 ? 100 : 50;
    var ticks = [];
    for (var t = Math.ceil(xmin / step) * step; t <= xmaxv; t += step) ticks.push(t);
    if (ticks.length) {
      var ax = ticks.map(X);
      out.push('<g stroke="' + pal.text + '" stroke-width="' + f(LWD) + '" fill="none">');
      out.push('<line x1="' + f(Math.min.apply(null, ax)) + '" y1="' + f(B) + '" x2="' + f(Math.max.apply(null, ax)) + '" y2="' + f(B) + '"/>');
      ax.forEach(function (x) { out.push('<line x1="' + f(x) + '" y1="' + f(B) + '" x2="' + f(x) + '" y2="' + f(B + 0.5 * LINE) + '"/>'); });
      out.push("</g>");
      var xKeep = keepLabels(ax, ticks.map(function () { return sAxis * 0.75; }), sAxis * 0.25);
      ticks.forEach(function (tv, k) {
        if (!xKeep[k]) return;
        var lab = bcad && tv === 0 ? "BCE/CE" : String(Math.abs(tv));
        out.push(text(X(tv) + sAxis * 0.35, B + LINE, lab, { size: sAxis, anchor: "end", rotate: -90 }));
      });
    }
    var xlab;
    if (bcad) {
      var a0 = Math.min(xlim[0], xlim[1]), a1 = Math.max(xlim[0], xlim[1]);
      xlab = a1 <= 0 ? "Years cal BCE" : a0 >= 0 ? "Years cal CE" : "Years cal BCE/CE";
    } else {
      xlab = "Years cal BP";
    }
    out.push(text((L + R) / 2, B + 4.5 * LINE + sLab * 1.0, xlab, { size: sLab, anchor: "middle" }));

    // ---- right axis: probability density ----
    var dTicks = pretty(uy[0], uy[1]).filter(function (v) { return v >= uy[0] - 1e-12 && v <= uy[1] + 1e-12; });
    var dLabs = formatTicks(dTicks);
    out.push('<g stroke="' + pal.text + '" stroke-width="' + f(LWD) + '" fill="none">');
    out.push('<line x1="' + f(R) + '" y1="' + f(Y(dTicks[0])) + '" x2="' + f(R) + '" y2="' + f(Y(dTicks[dTicks.length - 1])) + '"/>');
    dTicks.forEach(function (v) { out.push('<line x1="' + f(R) + '" y1="' + f(Y(v)) + '" x2="' + f(R + 0.5 * LINE) + '" y2="' + f(Y(v)) + '"/>'); });
    out.push("</g>");
    var dKeep = keepLabels(dTicks.map(Y), dLabs.map(function (s) { return measure(s, sAxis, false); }), measure("m", sAxis, false));
    dTicks.forEach(function (v, k) {
      if (!dKeep[k]) return;
      out.push(text(R + LINE + sAxis * 0.75, Y(v), dLabs[k], { size: sAxis, anchor: "middle", rotate: -90 }));
    });
    out.push(text(R + 4 * LINE + sLab * 0.75, (T + B) / 2, "Probability Density", { size: sLab, anchor: "middle", rotate: -90, fill: pal.text }));

    // ---- data layers, clipped to the plot region ----
    out.push('<g clip-path="url(#plotRegion)">');
    if (bcad && Math.min(xlim[0], xlim[1]) <= 0 && Math.max(xlim[0], xlim[1]) >= 0) {
      out.push('<line x1="' + f(X(0)) + '" y1="' + f(T) + '" x2="' + f(X(0)) + '" y2="' + f(B) + '" stroke="' + pal.marker + '" stroke-width="' + f(LWD) + '" stroke-dasharray="' + f(LWD) + "," + f(3 * LWD) + '" stroke-linecap="round"/>');
    }
    out.push('<polygon points="' + polyPoints(xvals.map(X), yvals.map(Y)) + '" fill="' + pal.main + '"' +
      (opts.bw ? ' stroke="#000000" stroke-width="' + f(LWD) + '"' : ' stroke="none"') + "/>");
    out.push("</g>");

    // ---- legend (topright, bty = "n") ----
    var hpdTxt = opts.hpd.map(function (h) {
      return bcad ? opts.fmtYear(h.startCalBP) + " - " + opts.fmtYear(h.endCalBP)
                  : h.startCalBP + " - " + h.endCalBP + " cal BP";
    });
    var meanTxt = "Mean: " + (bcad ? opts.fmtYear(opts.mean) : Math.round(opts.mean) + " cal BP");
    var legend = [{ s: "95.4% probability:", b: true }]
      .concat(hpdTxt.map(function (s) { return { s: s, b: false }; }))
      .concat([{ s: meanTxt, b: true }]);
    var maxW = 0;
    legend.forEach(function (l) { maxW = Math.max(maxW, measure(l.s, sLegend, l.b)); });
    var xchar = 0.15 * 100 * cexLabel;   // par("cin")[1] = 0.15 in
    var ychar = 0.2 * 100 * cexLabel;    // par("cin")[2] = 0.2 in
    var lx = R - maxW - 1.5 * xchar;
    legend.forEach(function (l, k) {
      var cy = T + ychar * (k + 1);
      out.push(text(lx, cy + sLegend * 0.35, l.s, { size: sLegend, bold: l.b, fill: pal.text }));
    });

    // ---- uncalibrated date on the radiocarbon axis ----
    var x0 = xlim[0], x15 = xlim[0] + (xlim[1] - xlim[0]) * 0.15;
    var cx = [], cy2 = [], pmax = 0, pmin = Infinity, probs = [];
    for (i = 0; i < 100; i++) {
      var c = cra - 4 * err + (8 * err) * i / 99;
      var z = (c - cra) / err;
      var p = Math.exp(-0.5 * z * z);
      probs.push(p); cy2.push(c);
      pmax = Math.max(pmax, p); pmin = Math.min(pmin, p);
    }
    for (i = 0; i < 100; i++) cx.push(x0 + (probs[i] - pmin) / (pmax - pmin) * (x15 - x0));
    out.push('<g clip-path="url(#plotRegion)">');
    var ux_ = [x0].concat(cx, [x0]).map(X);
    var uy_ = [cra - 4 * err].concat(cy2, [cra + 4 * err]).map(Y2);
    out.push('<polygon points="' + polyPoints(ux_, uy_) + '" fill="' + pal.uncal + '" stroke="none"/>');
    out.push('<polyline points="' + polyPoints(cx.map(X), cy2.map(Y2)) + '" fill="none" stroke="' + pal.uncal + '" stroke-width="' + f(LWD) + '"/>');
    out.push("</g>");

    // ---- left axis: radiocarbon age ----
    var cTicks = pretty(uy2[0], uy2[1]).filter(function (v) { return v >= uy2[0] && v <= uy2[1]; });
    var cLabs = formatTicks(cTicks);
    out.push('<g stroke="' + pal.text + '" stroke-width="' + f(LWD) + '" fill="none">');
    out.push('<line x1="' + f(L) + '" y1="' + f(Y2(cTicks[0])) + '" x2="' + f(L) + '" y2="' + f(Y2(cTicks[cTicks.length - 1])) + '"/>');
    cTicks.forEach(function (v) { out.push('<line x1="' + f(L) + '" y1="' + f(Y2(v)) + '" x2="' + f(L - 0.5 * LINE) + '" y2="' + f(Y2(v)) + '"/>'); });
    out.push("</g>");
    var cKeep = keepLabels(cTicks.map(Y2), cTicks.map(function () { return sAxis * 0.75; }), sAxis * 0.25);
    cTicks.forEach(function (v, k) {
      if (!cKeep[k]) return;
      out.push(text(L - LINE, Y2(v) + sAxis * 0.35, cLabs[k], { size: sAxis, anchor: "end" }));
    });
    out.push(text(L - 3.5 * LINE - sLab * 0.25, (T + B) / 2, "Radiocarbon Age (BP)", { size: sLab, anchor: "middle", rotate: -90 }));

    // ---- calibration curve ----
    var raw = opts.curve.raw;
    var bmin = grid.calBP[n - 1] - 100, bmax = grid.calBP[0] + 100;
    var ccx = [], ccm = [], ccu = [], ccl = [];
    for (i = 0; i < raw.bp.length; i++) {
      if (raw.bp[i] >= bmin && raw.bp[i] <= bmax) {
        ccx.push(X(toX(raw.bp[i])));
        ccm.push(Y2(raw.c14[i]));
        ccu.push(Y2(raw.c14[i] + raw.err[i]));
        ccl.push(Y2(raw.c14[i] - raw.err[i]));
      }
    }
    out.push('<g clip-path="url(#plotRegion)">');
    out.push('<polygon points="' + polyPoints(ccx.concat(ccx.slice().reverse()), ccu.concat(ccl.slice().reverse())) +
      '" fill="' + pal.curveShd + '"' + (pal.curveShdOpacity < 1 ? ' fill-opacity="' + pal.curveShdOpacity + '"' : "") + ' stroke="none"/>');
    out.push('<polyline points="' + polyPoints(ccx, ccm) + '" fill="none" stroke="' + pal.curve + '" stroke-width="' + f(1.5 * LWD) + '"' +
      (opts.bw ? ' stroke-dasharray="' + f(4 * 1.5 * LWD) + "," + f(4 * 1.5 * LWD) + '"' : "") + ' stroke-linejoin="round"/>');
    out.push("</g>");

    // Box (drawn last so it sits on top, as R's box() does visually)
    out.push('<rect x="' + f(L) + '" y="' + f(T) + '" width="' + f(R - L) + '" height="' + f(B - T) + '" fill="none" stroke="#000000" stroke-width="' + f(LWD) + '"/>');
    out.push("</svg>");
    return out.join("");
  }

  var api = { render: render, pretty: pretty, WIDTH: W, HEIGHT: H };
  if (typeof module !== "undefined" && module.exports) module.exports = api;
  else root.CPlot = api;
})(this);
