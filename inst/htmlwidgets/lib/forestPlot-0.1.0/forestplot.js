(function(global, factory) {
    typeof exports === 'object' && typeof module !== 'undefined'
        ? (module.exports = factory(require('d3')))
        : typeof define === 'function' && define.amd
        ? define(['d3'], factory)
        : ((global = global || self), (global.forestPlot = factory(global.d3)));
})(this, function(d3$1) {
    'use strict';

    if (typeof Object.assign != 'function') {
        Object.defineProperty(Object, 'assign', {
            value: function assign(target, varArgs) {
                if (target == null) {
                    // TypeError if undefined or null
                    throw new TypeError('Cannot convert undefined or null to object');
                }

                var to = Object(target);

                for (var index = 1; index < arguments.length; index++) {
                    var nextSource = arguments[index];

                    if (nextSource != null) {
                        // Skip over if undefined or null
                        for (var nextKey in nextSource) {
                            // Avoid bugs when hasOwnProperty is shadowed
                            if (Object.prototype.hasOwnProperty.call(nextSource, nextKey)) {
                                to[nextKey] = nextSource[nextKey];
                            }
                        }
                    }
                }

                return to;
            },
            writable: true,
            configurable: true
        });
    }

    if (!Array.prototype.find) {
        Object.defineProperty(Array.prototype, 'find', {
            value: function value(predicate) {
                // 1. Let O be ? ToObject(this value).
                if (this == null) {
                    throw new TypeError('"this" is null or not defined');
                }

                var o = Object(this);

                // 2. Let len be ? ToLength(? Get(O, 'length')).
                var len = o.length >>> 0;

                // 3. If IsCallable(predicate) is false, throw a TypeError exception.
                if (typeof predicate !== 'function') {
                    throw new TypeError('predicate must be a function');
                }

                // 4. If thisArg was supplied, let T be thisArg; else let T be undefined.
                var thisArg = arguments[1];

                // 5. Let k be 0.
                var k = 0;

                // 6. Repeat, while k < len
                while (k < len) {
                    // a. Let Pk be ! ToString(k).
                    // b. Let kValue be ? Get(O, Pk).
                    // c. Let testResult be ToBoolean(? Call(predicate, T, � kValue, k, O �)).
                    // d. If testResult is true, return kValue.
                    var kValue = o[k];
                    if (predicate.call(thisArg, kValue, k, o)) {
                        return kValue;
                    }
                    // e. Increase k by 1.
                    k++;
                }

                // 7. Return undefined.
                return undefined;
            }
        });
    }

    if (!Array.prototype.findIndex) {
        Object.defineProperty(Array.prototype, 'findIndex', {
            value: function value(predicate) {
                // 1. Let O be ? ToObject(this value).
                if (this == null) {
                    throw new TypeError('"this" is null or not defined');
                }

                var o = Object(this);

                // 2. Let len be ? ToLength(? Get(O, "length")).
                var len = o.length >>> 0;

                // 3. If IsCallable(predicate) is false, throw a TypeError exception.
                if (typeof predicate !== 'function') {
                    throw new TypeError('predicate must be a function');
                }

                // 4. If thisArg was supplied, let T be thisArg; else let T be undefined.
                var thisArg = arguments[1];

                // 5. Let k be 0.
                var k = 0;

                // 6. Repeat, while k < len
                while (k < len) {
                    // a. Let Pk be ! ToString(k).
                    // b. Let kValue be ? Get(O, Pk).
                    // c. Let testResult be ToBoolean(? Call(predicate, T, � kValue, k, O �)).
                    // d. If testResult is true, return k.
                    var kValue = o[k];
                    if (predicate.call(thisArg, kValue, k, o)) {
                        return k;
                    }
                    // e. Increase k by 1.
                    k++;
                }

                // 7. Return -1.
                return -1;
            }
        });
    }

    Math.log10 = Math.log10 =
        Math.log10 ||
        function(x) {
            return Math.log(x) * Math.LOG10E;
        };

    // https://github.com/wbkd/d3-extended
    d3$1.selection.prototype.moveToFront = function() {
        return this.each(function() {
            this.parentNode.appendChild(this);
        });
    };

    d3$1.selection.prototype.moveToBack = function() {
        return this.each(function() {
            var firstChild = this.parentNode.firstChild;
            if (firstChild) {
                this.parentNode.insertBefore(this, firstChild);
            }
        });
    };

    function layout() {
        var chart = this;
        chart.wrap = d3.select(chart.element).attr('class', 'forestplot');
        chart.controls = d3
            .select(chart.element)
            .append('div')
            .attr('class', 'controls');
    }

    function makeScales() {
        var chart = this;
        var config = this.config;

        chart.colorScale = d3.scale
            .ordinal()
            .range([
                '#999',
                '#e41a1c',
                '#377eb8',
                '#4daf4a',
                '#984ea3',
                '#ff7f00',
                '#ffff33',
                '#a65628',
                '#f781bf'
            ])
            .domain(
                chart.config.groups.map(function(m) {
                    return m.group;
                })
            );

        //Rate Scale - shared across tests
        var all_percents = chart.raw.map(function(m) {
            var num1 = +m.n1;
            var num2 = +m.n2;
            var denom1 = +m.N1;
            var denom2 = +m.N2;
            var percent1 = num1 / denom1;
            var percent2 = num2 / denom2;
            return [percent1, percent2];
        });
        var percent_extent = d3.extent(d3.merge(all_percents));

        chart.rateScale = d3.scale
            .linear()
            .range([10, 110])
            .domain(percent_extent);

        //Test scale - defined for each test
        chart.anly.forEach(function(testData) {
            var all_comparisons = d3.merge(
                testData.values.map(function(m) {
                    return m.values.comparison;
                })
            );
            var all_upper = all_comparisons.map(function(m) {
                return +m[config.result_upper_col];
            });
            var all_lower = all_comparisons.map(function(m) {
                return +m[config.result_lower_col];
            });
            var testExtent = [d3.min(all_lower), d3.max(all_upper)];
            //let testExtent = [0, d3.max(all_upper)];
            testData.testScale = d3.scale
                .linear()
                .range([10, config.comparisonWidth - 10])
                .domain(testExtent);
        });
    }

    function makeBody(testData) {
        var chart = this;
        var config = this.config;
        var table = testData;
        var wrap = testData.table;
        table.body = wrap.append('tbody');
        table.rows = table.body
            .selectAll('tr.value')
            .data(testData.values)
            .enter()
            .append('tr')
            .attr('class', 'value');
        table.rows
            .append('td')
            .attr('class', 'soc')
            .text(function(d) {
                var hl = d.values.high_level_cat;
                return hl.length > 25 ? hl.substring(0, 25) + '...' : hl;
            })
            .attr('title', function(d) {
                return d.values.high_level_cat;
            });
        table.rows
            .append('td')
            .attr('class', 'term')
            .text(function(d) {
                var ll = d.values.low_level_cat;
                return ll.length > 25 ? ll.substring(0, 25) + '...' : ll;
            })
            .attr('title', function(d) {
                return d.values.low_level_cat;
            });

        //Group Counts

        table.rows
            .selectAll('td.group-count')
            .data(function(d) {
                return d.values.group;
            })
            .enter()
            .append('td')
            .attr('class', 'group-count')
            .style('text-align', 'center')
            .text(function(d) {
                return d.percent_text;
            })
            .attr('title', function(d) {
                return d.numerator + '/' + d.denominator;
            })
            .style('cursor', 'help')
            .style('color', function(d) {
                return chart.colorScale(d.group);
            })
            .classed('hidden', config.hideCounts);

        //group plot
        table.groupPlot = table.rows
            .append('td')
            .attr('class', 'group-plot plot')
            .append('svg')
            .attr('height', 20)
            .attr('width', 120);
        table.groupPlot
            .selectAll('circle')
            .data(function(d) {
                return d.values.group;
            })
            .enter()
            .append('circle')
            .attr('cx', function(d) {
                return chart.rateScale(d.percent);
            })
            .attr('cy', 10)
            .attr('r', 5)
            .attr('stroke', function(d) {
                return chart.colorScale(d.group);
            })
            .attr('fill', function(d) {
                return chart.colorScale(d.group);
            })
            .style('cursor', 'help')
            .append('title')
            .text(function(d) {
                return (
                    d.group + ': ' + d.percent_text + ' (' + d.numerator + '/' + d.denominator + ')'
                );
            });

        //Group Comparisons
        table.rows
            .selectAll('td.compare')
            .data(function(d) {
                return d.values.comparison;
            })
            .enter()
            .append('td')
            .attr('class', 'compare')
            .style('text-align', 'center')
            .text(function(d) {
                return d.result_text;
            })
            .attr('title', function(d) {
                return (
                    'p=' +
                    d[config.p_col] +
                    ', confidence interval=[' +
                    d[config.result_lower_col] +
                    ',' +
                    d[config.result_upper_col] +
                    ']'
                );
            })
            .style('font-weight', function(d) {
                return d[config.p_col] < 0.05 ? 'bold' : null;
            })
            .style('color', function(d) {
                return d[config.p_col] < 0.05 ? 'black' : '#ccc';
            });

        var diffPlots = table.rows
            .append('td')
            .attr('class', 'diffplot plot')
            .append('svg')
            .attr('height', function(d) {
                return (
                    20 *
                    d.values.comparison.filter(function(f) {
                        return f.result_text != '-';
                    }).length
                );
            })
            .attr('width', config.comparisonWidth)
            .append('g');

        var diffPoints = diffPlots
            .selectAll('g')
            .data(function(d) {
                return d.values.comparison
                    .filter(function(f) {
                        return f.result_text != '-';
                    })
                    .filter(function(d) {
                        return !isNaN(d[config.result_upper_col]);
                    })
                    .filter(function(d) {
                        return !isNaN(d[config.result_lower_col]);
                    });
            })
            .enter()
            .append('g')
            .attr('class', 'diffg')
            .attr('transform', function(d, i) {
                return 'translate(0, ' + i * 15 + ')';
            })
            .attr('cursor', 'help');

        diffPoints
            .append('line')
            .attr('class', 'ci')
            .attr('x1', function(d) {
                return table.testScale(d[config.result_upper_col]);
            })
            .attr('x2', function(d) {
                return table.testScale(d[config.result_lower_col]);
            })
            .attr('y1', 20 / 2)
            .attr('y2', 20 / 2)
            .attr('stroke-width', '1px')
            .attr('stroke', 'black')
            .attr('opacity', '0.4');

        //diffPoints.append('title').text(d => d[config.group1_col]+" vs. " + ': ' + d.or + ' (p=' + d.p + ')');

        /* Append graphical rate differences.*/
        var triangle = d3.svg
            .line()
            .x(function(d) {
                return d.x;
            })
            .y(function(d) {
                return d.y;
            })
            .interpolate('linear-closed');

        diffPoints
            .append('svg:path')
            .attr('d', function(d) {
                var h = 20,
                    r = 5;

                var leftpoints = [
                    { x: table.testScale(d[config.result_col]), y: h / 2 + r }, //bottom
                    { x: table.testScale(d[config.result_col]) - r, y: h / 2 }, //middle-left
                    {
                        x: table.testScale(d[config.result_col]),
                        y: h / 2 - r //top
                    }
                ];
                return triangle(leftpoints);
            })
            .attr('class', 'diamond')
            .attr('fill-opacity', function(d) {
                return d[config.p_col] < 0.05 ? 1 : 0.1;
            })
            .attr('fill', function(d) {
                return chart.colorScale(d[config.group1_col]);
            })
            .attr('stroke', function(d) {
                return chart.colorScale(d[config.group2_col]);
            })
            .attr('stroke-opacity', 0.3);

        diffPoints
            .append('svg:path')
            .attr('d', function(d) {
                var h = 20;
                var r = 5;

                var rightpoints = [
                    { x: table.testScale(d[config.result_col]), y: h / 2 + r }, //bottom
                    { x: table.testScale(d[config.result_col]) + r, y: h / 2 }, //middle-right
                    {
                        x: table.testScale(d[config.result_col]),
                        y: h / 2 - r //top
                    }
                ];
                return triangle(rightpoints);
            })
            .attr('class', 'diamond')
            .attr('fill-opacity', function(d) {
                return d[config.p_col] < 0.05 ? 1 : 0.1;
            })
            .attr('fill', function(d) {
                return chart.colorScale(d[config.group2_col]);
            })
            .attr('stroke', function(d) {
                return chart.colorScale(d[config.group2_col]);
            })
            .attr('stroke-opacity', 0.3);

        diffPoints.append('title').text(function(d) {
            var p = +d.Pvalue < 0.01 ? '<0.01' : '' + parseFloat(d.Pvalue).toFixed(2);
            return (
                d.comp +
                ' - ' +
                d.Test +
                ': ' +
                parseFloat(d.Res).toFixed(2) +
                ' [' +
                parseFloat(d.CI_Lower).toFixed(2) +
                ', ' +
                parseFloat(d.CI_Upper).toFixed(2) +
                '], p: ' +
                p
            );
        });
    }

    function makeHeader(testData) {
        var chart = this;
        var config = this.config;
        var table = testData;
        var wrap = testData.table;

        table.head = wrap.append('thead').style('text-align', 'center');
        table.head1 = table.head.append('tr');
        table.head1.append('th');
        table.head1.append('th');
        table.head1
            .append('th')
            .text('Groups')
            .attr('class', 'groupHead')
            .attr('colspan', config.hideCounts ? 1 : config.groups.length + 1);
        table.head1
            .append('th')
            .text(testData.key)
            .attr('colspan', config.pairs.length + 1);

        table.head2 = table.head.append('tr');
        table.head2.append('th').text('System Organ Class');
        table.head2.append('th').text('Preferred Term');
        table.head2
            .selectAll('th.group')
            .data(config.groups)
            .enter()
            .append('th')
            .attr('class', 'group')
            .text(function(d) {
                return d.group;
            })
            .classed('hidden', config.hideCounts);

        var rateAxis = d3.svg
            .axis()
            .scale(chart.rateScale)
            .ticks(3)
            .tickFormat(function(d) {
                return d * 100;
            })
            .orient('top');

        table.head2
            .append('th')
            .text('Percentage')
            .attr('class', 'rates axis')
            .append('svg')
            .attr('height', 20)
            .attr('width', 120)
            .append('svg:g')
            .attr('class', 'axis percent')
            .attr('transform', 'translate(0,20)')
            .call(rateAxis);

        table.head2
            .selectAll('th.pairs')
            .data(config.pairs)
            .enter()
            .append('th')
            .text(function(d) {
                return d;
            });
        var testAxis = d3.svg
            .axis()
            .scale(testData.testScale)
            .ticks(6)
            .orient('top');

        table.head2
            .append('th')
            .text('Comparison')
            .attr('class', 'diffs axis')
            .append('svg')
            .attr('height', '20')
            .attr('width', config.comparisonWidth)
            .append('svg:g')
            .attr('class', 'axis percent')
            .attr('transform', 'translate(0,20)')
            .call(testAxis);
    }

    function draw() {
        var chart = this;

        chart.anly.forEach(function(testData, i) {
            testData.wrap = chart.wrap
                .append('div')
                .attr('class', 'tableWrap')
                .classed('hidden', i > 0)
                .datum(testData.key);
            testData.wrap.append('h2').text(testData.key);
            testData.table = testData.wrap
                .append('table')
                .attr('class', 'table ae-table table' + i);
            makeHeader.call(chart, testData);
            makeBody.call(chart, testData);
            $('.forestplot .tableWrap .table' + i)
                .DataTable({
                    dom: '<"top"if>rt<"clear">',
                    paging: false,
                    order: [[2, 'desc']],
                    columnDefs: [
                        { width: '120px', targets: 2 + chart.config.groups.length },
                        {
                            width: '300px',
                            targets: 2 + chart.config.groups.length + 1 + chart.config.pairs.length
                        }
                    ]
                })
                .columns.adjust()
                .draw();
        });
    }

    function processData() {
        var chart = this;
        var config = this.config;

        //get list of all groups
        var allgroups = d3.merge(
            chart.raw.map(function(m) {
                return [
                    { group: m[config.group1_col], count: m[config.denominator1_col] },
                    { group: m[config.group2_col], count: m[config.denominator2_col] }
                ];
            })
        );

        var group_names = d3
            .set(
                allgroups.map(function(m) {
                    return m.group;
                })
            )
            .values()
            .sort();

        config.groups = group_names.map(function(name) {
            var count = allgroups.filter(function(f) {
                return f.group == name;
            })[0].count;
            return { group: name, count: +count };
        });

        //get list of all comparisons
        var allcomps = chart.raw.map(function(m) {
            var comp = m[config.group1_col] + ':' + m[config.group2_col];
            return comp;
        });

        config.pairs = d3
            .set(allcomps)
            .values()
            .sort();

        //make nested data for analysis
        this.anly = d3
            .nest()
            .key(function(f) {
                return f[config.test_col];
            }) // group by test type
            .key(function(f) {
                return f[config.high_level_col] + ':' + f[config.low_level_col];
            }) // and AE type
            .rollup(function(pt) {
                var groups = d3
                    .nest()
                    .key(function(f) {
                        return f[config.group1_col];
                    })
                    .rollup(function(d) {
                        return {
                            group: d[0][config.group1_col],
                            numerator: +d[0][config.numerator1_col],
                            denominator: +d[0][config.denominator1_col]
                        };
                    })
                    .entries(pt)
                    .map(function(f) {
                        return f.values;
                    });

                var comparison_groups = d3
                    .nest()
                    .key(function(f) {
                        return f[config.group2_col];
                    })
                    .rollup(function(d) {
                        return {
                            group: d[0][config.group2_col],
                            numerator: +d[0][config.numerator2_col],
                            denominator: +d[0][config.denominator2_col]
                        };
                    })
                    .entries(pt)
                    .map(function(f) {
                        return f.values;
                    });

                //add unused comparison groups to group list
                var group_names = groups.map(function(m) {
                    return m.group;
                });
                comparison_groups.forEach(function(comp_group) {
                    if (group_names.indexOf(comp_group.group) == -1) {
                        groups.push(comp_group);
                    }
                });

                // Add in missing groups using config
                var all_group_names = groups.map(function(m) {
                    return m.group;
                });
                config.groups.forEach(function(config_group) {
                    if (all_group_names.indexOf(config_group.group) == -1) {
                        groups.push({
                            group: config_group.group,
                            numerator: 0,
                            denominator: config_group.count
                        });
                    }
                });

                // calculate percents
                groups.forEach(function(g) {
                    g.percent = g.numerator / g.denominator;
                    g.percent_text = d3.format('0.1%')(g.percent);
                });

                groups.sort(function(a, b) {
                    if (a.group < b.group) {
                        return -1;
                    }
                    if (a.group > b.group) {
                        return 1;
                    }
                    return 0;
                });

                // Get comparisons
                var comparisons = d3
                    .nest()
                    .key(function(f) {
                        return f[config.group1_col] + ':' + f[config.group2_col];
                    })
                    .entries(pt)
                    .map(function(f) {
                        return f.values[0];
                    });

                comparisons.forEach(function(d) {
                    d.comp = d[config.group1_col] + ':' + d[config.group2_col];
                    d.result_text = d3.format('0.2f')(d[config.result_col]);
                });

                var comp_names = comparisons.map(function(comp) {
                    return comp[config.group1_col] + ':' + comp[config.group2_col];
                });

                config.pairs.forEach(function(config_pair) {
                    if (comp_names.indexOf(config_pair) == -1) {
                        comparisons.push({
                            comp: config_pair,
                            result_text: '-'
                        });
                    }
                });

                comparisons.sort(function(a, b) {
                    if (a.comp < b.comp) {
                        return -1;
                    }
                    if (a.comp > b.comp) {
                        return 1;
                    }
                    return 0;
                });

                var shell = {
                    high_level_cat: pt[0][config.high_level_col],
                    low_level_cat: pt[0][config.low_level_col],
                    group: groups,
                    comparison: comparisons
                };
                return shell;
            })
            .entries(this.raw);
    }

    function makeTestControl() {
        var tests = this.anly.map(function(m) {
            return m.key;
        });
        var wrap = this.controls.append('div').attr('class', 'slider-wrap');

        wrap.append('span')
            .attr('class', 'label')
            .text('Select Test');
        wrap.append('br');
        var test_control = wrap.append('select');
        test_control
            .selectAll('option')
            .data(tests)
            .enter('append')
            .append('option')
            .text(function(d) {
                return d;
            });
        test_control.on('change', function() {
            var current = this.value;
            d3.selectAll('div.tableWrap').classed('hidden', function(d) {
                return d != current;
            });
        });
    }

    function makeCountToggle() {
        var wrap = this.controls.insert('div', '*').attr('class', 'slider-wrap');
        var charts = this;
        var config = charts.config;
        wrap.append('span')
            .attr('class', 'label')
            .text('Show Rates');
        wrap.append('br');
        var test_control = wrap.append('input').attr('type', 'checkbox');
        test_control.on('change', function() {
            var current = this.checked;
            charts.config.hideGroups = !this.checked;
            charts.anly.forEach(function(chart) {
                chart.head1
                    .selectAll('th.groupHead')
                    .attr('colspan', config.hideGroups ? 1 : config.groups.length + 1);
                chart.head2.selectAll('th.group').classed('hidden', config.hideGroups);
                chart.body.selectAll('td.group-count').classed('hidden', config.hideGroups);
            });
        });
    }

    function forestPlot(data) {
        var element = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : 'body';
        var settings = arguments[2];

        var chart = {
            raw: data,
            element: element,
            config: settings
        };

        layout.call(chart);
        processData.call(chart);
        makeScales.call(chart);
        makeTestControl.call(chart);
        draw.call(chart);
        //makeFilterControls.call(chart, chart.anly[0]);
        makeCountToggle.call(chart);
    }

    return forestPlot;
});
