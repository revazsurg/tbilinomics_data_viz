const projection = d3.geoMercator()
    .translate([width/2, height/2])
    .scale([5000])
    .center([43.39, 42.3]);

const path = d3.geoPath()
    .projection(projection);

const color = d3.scaleQuantile()
    .range(["#ffffb2", "#fecc5c", "#fd8d3c", "#e31a1c"]);

color.domain([d3.min(data, d => d.Indicator), 
    d3.max(data, d => d.Indicator)]);

const div = d3.select("body")
    .append("div")
    .attr("class", "tooltip")
    .style("opacity", 0);

d3.json("georgia_munis.geojson").then(json => {
    
    for(let d of data) {
        
        const dataMuni = d.Municipality;
        const dataValue = d.Indicator;
    
        for(let feature of json.features) {
            
            const jsonMuni = feature.properties.NAME_2;
            
            if(dataMuni == jsonMuni) {
                feature.properties.Indicator = dataValue;
                break;
            }
        }
    }
    
    const map = svg.selectAll("path").data(json.features);

    map.enter()
        .append("path")
        .attr("d", path)
        .attr("stroke", "gray")
        .style("fill", d => {
            const indicator = d.properties.Indicator;
                
            if (indicator) {
                return color(indicator);
            } else {
                return "#ccc";
            }
        })
        .style("opacity", 0.8)
        
        .on("mouseover", (d, i, n) => {
            d3.select(n[i])
                .transition()
                .duration(300)
                .style("opacity", 1);
            div.transition()
                .duration(300)
                .style("opacity", 1)
                .text(d.properties.NAME_2 + " : " + d.properties.Indicator)
                .style("left", (d3.event.pageX) + "px")
                .style("top", (d3.event.pageY - 30) + "px");
        })
        .on("mouseout", (d, i, n) => {
            d3.select(n[i])
                .transition()
                .duration(300)
                .style("opacity", 0.8);
            div.transition()
                .duration(300)
                .style("opacity", 0);
        });

    map.transition()
        .duration(1000)
        .style("fill", d => {
            const indicator = d.properties.Indicator;
                
            if (indicator) {
                return color(indicator);
            } else {
                return "#ccc";
            }
        })
        .style("opacity", 0.8);
}); 

const min = d3.min(data, d => d.Indicator);
const max = d3.max(data, d => d.Indicator);

const colorDomain = [
    25, 50, 75
];

const extColorDomain = [
    0, 25, 50, 75
];

const legendLabels = [
    "0 - 25",
    "25 - 50",
    "50 - 75",
    "75 - 100"
];

const colorLegend = d3.scaleThreshold()
    .domain(colorDomain)
    .range(["#ffffb2", "#fecc5c", "#fd8d3c", "#e31a1c"]);

const legend = svg.selectAll("g.legend")
    .data(extColorDomain)
    .enter()
    .append("g")
    .attr("class", "legend");

const lsW = 20;
const lsH = 20;

legend.append("rect")
    .attr("x", width - 100)
    .attr("y", (d, i) => lsH + (i * lsH) )
    .attr("width", lsW)
    .attr("height", lsH)
    .style("fill", (d, i) => colorLegend(d))
    .style("opacity", 0.8);

legend.append("text")
    .attr("x", width - 70)
    .attr("y", (d, i) => lsH + 15 + (i * lsH))
    .text((d, i) => legendLabels[i]);