import sys

import pandas as pd
import plotly
import plotly.graph_objs as go
from scipy.stats import ks_2samp, ttest_ind

interest = ['People Taking Action', 'Impressions', 'Reach', 'Post Reactions', 'Relevance Score', 'Frequency',
            'Social Impressions']  # variables of interest

lifetime = pd.read_csv(sys.argv[1])

campaigns = []
names = []
for x in lifetime['Campaign Name'].unique().tolist()[:-1]:  # last value is the pilot campaign
    campaigns.append(lifetime[lifetime['Campaign Name'] == x])
    names.append(x)

methods = {}


def ks_mat(var):
    methods[ks_mat] = "KS Test"
    m = []
    for ro in range(len(names)):
        row = []
        for co in range(len(names)):
            row.append(ks_2samp(campaigns[ro][var], campaigns[co][var])[1])
        m.append(row)
    return m


def t_mat(var):
    methods[t_mat] = "T Test"
    m = []
    for ro in range(len(names)):
        row = []
        for co in range(len(names)):
            row.append(ttest_ind(campaigns[ro][var], campaigns[co][var])[1])
        m.append(row)
    return m


def heat(var, meth=ks_mat, ret=False):
    fig = go.Figure(data=[go.Heatmap(x=names, y=names, z=meth(var), colorscale='Jet', name=var)],
                    layout=go.Layout(title=var + " " + methods[meth] + " Probability Heatmap"))
    if ret:
        return fig
    else:
        plotly.offline.plot(fig,
                            filename=var.replace(" ", "_") +
                                     '_' + methods[meth].replace(" ", "_") +
                                     '_probabilities_heatmap.html',
                            show_link=False)


def hist(var, ret=False):
    fig = go.Figure(data=[go.Histogram(x=c[var],
                                       opacity=0.25,
                                       name=c['Campaign Name'].iloc[0]) for c in campaigns],
                    layout=go.Layout(title="Overlaid Histograms of " + var,
                                     barmode='overlay'))
    if ret:
        return fig
    else:
        plotly.offline.plot(fig, filename=var.replace(" ", "_") + "_overlaid_hist.html", show_link=False)


def bar(values, barnames, name, ret=False):
    fig = go.Figure(
        data=[go.Bar(
            x=barnames,
            y=values
        )],
        layout=go.Layout(
            title=name
        )
    )
    if ret:
        return fig
    else:
        plotly.offline.plot(fig, filename=name + ".html", show_link=False)


def correlation_matrix(df, name="Correlation Matrix", ret=False):
    labels = df.columns.tolist()
    out_list = []
    for index, row in df.corr().iterrows():
        out_list.append(row.tolist())
    fig = go.Figure(
        data = [go.Heatmap(x=labels, y=labels, z=out_list, colorscale='Jet')],
        layout = go.Layout(title=name)
    )
    if ret:
        return fig
    else:
        plotly.offline.plot(fig, filename=name+".html", show_link=False)



def multiplot(plots, name="Summary", save=True):
    html = ""
    for plot in plots:
        html = html + plotly.offline.plot(plot, output_type="div", show_link=False)
    if save:
        with open(name + ".html", "w") as f:
            f.write(html)
    else:
        return html

def columnar_multiplot(traces, name="Summary", num_cols=2):
    c = 1
    r = 1
    for t in traces:
        fig.append_trace(t, r, c)
        c += 1
        if c > num_cols:
            c = 1
            r += 1
    if save:
        plotly.offline.plot(fig, filename=name+".html", show_link=False)

def stacked_bar(vals, barnames, tracenames, name="stacked_bar", ret=False):
    traces = [go.Bar(x=barnames, y=vs, name=t) for vs in vals for t in tracenames]
    fig = go.Figure(
        data=traces,
        layout=go.Layout(
            barmode='stack'
        )
    )
    if ret:
        return fig
    else:
        plotly.offline.plot(fig, filename=name+".html")

plots = []
for v in interest:
    print("Graphing "+v)
    plots.append(bar([c[v].sum() for c in campaigns], names, v+" by Campaign (Sum)", ret=True))
    plots.append(bar([c[v].mean() for c in campaigns], names, v+" by Campaign (Average)", ret=True))
    plots.append(bar([c[v].median() for c in campaigns], names, v+" by Campaign (Median)", ret=True))
    plots.append(heat(v, ks_mat, ret=True))
    plots.append(heat(v, t_mat, ret=True))
    for i in range(-1, len(plots) - 2):
        assert type(plots[i]) == type(plots[i + 1])
plots.append(correlation_matrix(lifetime[interest], ret=True))

siteage = pd.read_csv("/Users/dominicburkart/Downloads/Analytics All Web Site Data Demographics- Age 20170601-20171029.csv")


l = list(siteage['Page'].unique())
agelist = []
agenames = ["Total", "25-34", "35-44", "18-24", "45-54", "55-64"]
for u in l:
    df = siteage[siteage['Page'] == u]
    agelist.append([df[v].sum() for v in agenames])

agebars = []
for i in range(len(agenames)):
    agebars.append([agelist[l][i] for l in range(len(agelist))])

# plots.append(
#     stacked_bar(
#         agebars,
#         l,
#         agenames,
#         ret=True
#     )
# )

multiplot(plots, save=True)
