``` r
#-----------------------------------------------------------------------------------------#
# NEEDED PACKAGES
#-----------------------------------------------------------------------------------------#
library(markovifyR)
library(markovchain)
library(dplyr)
library(tidytext)
library(tidyverse)
library(tm)
library(purrr)
library(stringr)
library(textclean)
library(kableExtra)
library("extrafont")

# font_import(pattern="[O/o]swald")
# font_import(pattern="PT_Sans")
# font_import(pattern="[R/r]aleway")
# loadfonts(device="win")

# system("pip install markovify")
# devtools::install_github("abresler/markovifyR")
## https://github.com/abresler/markovifyR
```

Much hype is generated from technology consulting firms and vendors.
This has led firms like Gartner to publish the now popular [Hype
Cycle](https://en.wikipedia.org/wiki/Hype_cycle).

This is a **humorous collection** of (loosely) convincing sentences I
generated based on training data found on the website of 5 popular
technology vendor and consulting firms. Of course, the selection of the
training data is biased, as I only looked for areas where buzzwords are
common.

The generator uses a [Markov
chain](https://en.wikipedia.org/wiki/Markov_chain), a true random
process model, not another meaningless term.

``` r
## Load data
# cname <- file.path("F:/GoogleDrive/Personal_Work/Gibberish_Generator/Data")

cname <- file.path("/home/tspeidel/GoogleDrive/Personal_Work/Gibberish_Generator/Data")

docs <- VCorpus(DirSource(cname))
docs <-data.frame(text = unlist(sapply(docs, `[`, "content")), stringsAsFactors = F)


## Cleanup
docs <- docs %>%
  filter(cumsum(text == "") > 0,
         cumsum(str_detect(text, "^--")) == 0) %>%
  drop_empty_row() %>%
  tibble::rowid_to_column("id")

docs$text <- strip(docs$text, char.keep = c("?", ",", ";", "."))
docs$text <- replace_contraction(docs$text)
docs$text <- replace_html(docs$text)
docs$text <- replace_non_ascii(docs$text)
docs$text <- replace_number(docs$text, remove=TRUE)
docs$text <- replace_symbol(docs$text)
docs$text <- replace_white(docs$text)



## Markov model
buzz <- docs %>% pull(text)

markov_model <-
  generate_markovify_model(
    input_text = buzz,
    markov_state_size = 2L,
    max_overlap_total = 25,
    max_overlap_ratio = .85
  )


buzz.out <- markovify_text(
  markov_model = markov_model,
  maximum_sentence_length = NULL,
  output_column_name = "Gibberish",
  count = 30,
  tries = 200,
  only_distinct = TRUE,
  return_message = FALSE
)
```

<br>

Output
======

And here’s the output. It needs some more cleaning (capitalization,
punctuation and more verbs):

``` r
kable(buzz.out, "html", align = "l", digits = 1) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F, position = "left")
```

<table class="table table-striped table-hover table-condensed" style="width: auto !important; ">
<thead>
<tr>
<th style="text-align:left;">
idRow
</th>
<th style="text-align:left;">
Gibberish
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
1
</td>
<td style="text-align:left;">
patricia staino for making manufacturing operations are executed against
any data that has institutional knowledge and designing for better
decision making.
</td>
</tr>
<tr>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
the nonlinear connections of neurons in the context is gained. a carta
approach embraces the reality of qc being explored across a wide variety
of channels to create assets that were trained to use a googlelike
search to easily analyze billions of data stewardship, organizations
should continue to use an immutable shared ledger platform. for example,
in the midterms. i know it if you believe this, where they had some
great help. i will say this, folks, everything that is lightning fast
and effective.
</td>
</tr>
<tr>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
dana kublin for continued singular brilliance, leading all things
digital. from there, nextgeneration transaction and recordkeeping
mechanisms. they can serve up a web browser, your initial destination is
likely no longer have to spend more time performing advanced analysis.
</td>
</tr>
<tr>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
why now? what is possible in these missioncritical functions.
</td>
</tr>
<tr>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
about deloitte insights is complete, very little latency. why would i
want more.
</td>
</tr>
<tr>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
geoff tuff is a software component. if we continue to evolve their
thinking away from project to apifocused development, they will provide
the needed agility to engineer new capabilities and service levels
within a webpage to automatically
</td>
</tr>
<tr>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
big data and are extremely expensive.
</td>
</tr>
<tr>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
carefully vet thirdparty capabilities and that can be solved with a set
of tools that support microservices is expected to reach out and
correctly, because if i would have otherwise been missed by humans. they
use an approach that distributes functionality to the tools and systems
examined in this situation as well.
</td>
</tr>
<tr>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
levels of automated capture and analysis of large data sets, inferring
data types, identifying hierarchies and relationships with loadcal
champions in each of these is that they have.
</td>
</tr>
<tr>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
we want to protect the american association for artificial intelligence,
darpatv, february , .
</td>
</tr>
<tr>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
after enjoying several years ago, when routh was cio for north america
is a boardroom conversation, an event driven by advances in ai,
particularly in southern europe, we are firmly in the enterprise
seradvice bus and add context to map data content to customer
satisfaction, helping our clients solve some of the innovation could
introduce, group and understand things. you should mitigate the risk of
leaving is x. n customers are using sap cloud platform
</td>
</tr>
<tr>
<td style="text-align:left;">
12
</td>
<td style="text-align:left;">
accenture is becoming more pressing.
</td>
</tr>
<tr>
<td style="text-align:left;">
13
</td>
<td style="text-align:left;">
laissezfaire. decisionmakers accepted that data without causing
decoherence. this represents a radical new paradigm that may lead to his
familiar life.
</td>
</tr>
<tr>
<td style="text-align:left;">
14
</td>
<td style="text-align:left;">
lessons from the top priority insights pushed to the buildout of the
nocollar workforce
</td>
</tr>
<tr>
<td style="text-align:left;">
15
</td>
<td style="text-align:left;">
in figure . example of doing things right e.g., execution versus doing
the wrong thing. but we should do that, hopefully, we re going to say
the individual team’s learner models, eventually resulting in the
policies and laws that prevent anarchy and enable rapid updates when new
data management rules had to pay because they wanted to enhance data
context, particularly in unstrucadtured data such as dwave. although not
yet available from vendors. this is only a new metric called quantum
volume. the metric accounts for such entities to trigger contract
execution. smart contracts allow for vastly accelerated development of
new technologies to be careful to distinguish what seems inteladligent
from what is the ecosystem workforce design for new features. our goal
was to a standalone algorithmic platform service, ml is thought of as
programming or supervised learning. for example, the conversational
platform to create new value will come from various industry segments.
</td>
</tr>
<tr>
<td style="text-align:left;">
16
</td>
<td style="text-align:left;">
what roles do recent technology innovations like machine learning to
improve existing products and services are used to create business
ecosystems.
</td>
</tr>
<tr>
<td style="text-align:left;">
17
</td>
<td style="text-align:left;">
we are spending your time, how you can call it terrorism. you can call
it murder. you can begin transforming themselves into nimble,
fastmoving, dynamic organizations better positioned to solve. for more
processing and content collection and delivery models, risk and
innovation as nothing more than a decade ago, we started with digital
transformation?
</td>
</tr>
<tr>
<td style="text-align:left;">
18
</td>
<td style="text-align:left;">
over the last few years, virtually every existing technology and
methodadologies that we never knew. but we really owe it to additional
pinboards.
</td>
</tr>
<tr>
<td style="text-align:left;">
19
</td>
<td style="text-align:left;">
digital reality technologies could become a thing you do. companies are
already yielding benefits to our people, don t look to break the codes
currently protecting networks and data. users upload and generate their
own bot.
</td>
</tr>
<tr>
<td style="text-align:left;">
20
</td>
<td style="text-align:left;">
digital natives typically solve a problem, the more intensive and
impactful opportunities found with the proliferation of new programs
that mimicked basic intelligence as they do, convergence of finance
portfolio management services. he also leads deloitte consulting llp and
leads the power to educate and train them on how to achieve for the
quantum architecture with the smarts of a progression to ai. hypothesize
and experiment with qc architectures and implementation of an ml
workflow system with a speaker or text on a unique physical object see
note .
</td>
</tr>
<tr>
<td style="text-align:left;">
21
</td>
<td style="text-align:left;">
quickly identify real opportunities and coping with changing business
demands.this is what gartner calls bimodal it.
</td>
</tr>
<tr>
<td style="text-align:left;">
22
</td>
<td style="text-align:left;">
one dimension of the worker coadhort.
</td>
</tr>
<tr>
<td style="text-align:left;">
23
</td>
<td style="text-align:left;">
in addition to starting early, deakin was able to handle increasingly
complex tasks whilst also making interactions that serve up replicated
facial expressions, gesticulations, and holograms in real time, the
questions are less structured and unstructured sources, such as
chatbots, help drive ai initiatives that require big data analytics
leader
</td>
</tr>
<tr>
<td style="text-align:left;">
24
</td>
<td style="text-align:left;">
creating a data strategy for deloitte north west europe, as well, and
our partners.
</td>
</tr>
<tr>
<td style="text-align:left;">
25
</td>
<td style="text-align:left;">
what roles do recent technology innovations like machine learning, and
growing.
</td>
</tr>
<tr>
<td style="text-align:left;">
26
</td>
<td style="text-align:left;">
building on the company is headquartered in palo alto, with officesin
seattle, london and bangalore.for more information on these labels of
enterprise services and features such as imadages, audio, video, and the
other side of the business meets the future perhaps within a document
for example, medical product manufacturer
</td>
</tr>
<tr>
<td style="text-align:left;">
27
</td>
<td style="text-align:left;">
now you got to help.
</td>
</tr>
<tr>
<td style="text-align:left;">
28
</td>
<td style="text-align:left;">
combining multiple individual technological innovations to meet every
need from transformation through leadership development, and automation,
espeadcially as data grows exponentially in both compute and other
technologies in the future of retail, and in order to deliver new
business value see figure . qubit timeline estimates
</td>
</tr>
<tr>
<td style="text-align:left;">
29
</td>
<td style="text-align:left;">
apart from a busiadness perspective? and importantly, do you keep the
traffic and processing data through multilayered neural networks and
systems and technical security requirements must be in the new talent
models seems daunting especially if your company culture is grounded in
humans working in the middle. in such a bad batch of parts, or if
production problems require higherthanexpected consumption, you run a
successful platform businesses and things.
</td>
</tr>
<tr>
<td style="text-align:left;">
30
</td>
<td style="text-align:left;">
experiment and expand this fundamental new technology. here are a lot of
promise, but it is a vertically suspended quantum computing becomes a
fundamental change in every core function, but perhaps none more so in
the right thing to have a lot of evil. a lot of people remembered and
they work and migrate resources to core offeradings, percent to adjacent
efforts, and aligns to business problems, and bold approaches to
handling batches of records, and inaccuracies. consider creating
internalexternal develadoper forums to encourage broader discovery and
implementation of quantum computing system dwave
</td>
</tr>
</tbody>
</table>
<br>

Spicing it up with Trump
========================

What if we add transcripts of Donald Trump speeches and tweets to spice
things up a bit?

``` r
## Load data
# cname <- file.path("F:/GoogleDrive/Personal_Work/Gibberish_Generator/Data")

cname <- file.path("/home/tspeidel/GoogleDrive/Personal_Work/Gibberish_Generator/Data2")

docs <- VCorpus(DirSource(cname))
docs <-data.frame(text = unlist(sapply(docs, `[`, "content")), stringsAsFactors = F)


## Cleanup
docs <- docs %>%
  filter(cumsum(text == "") > 0,
         cumsum(str_detect(text, "^--")) == 0) %>%
  drop_empty_row() %>%
  tibble::rowid_to_column("id")

docs$text <- strip(docs$text, char.keep = c("?", ",", ";", "."))
docs$text <- replace_contraction(docs$text)
docs$text <- replace_html(docs$text)
docs$text <- replace_non_ascii(docs$text)
docs$text <- replace_number(docs$text, remove=TRUE)
docs$text <- replace_symbol(docs$text)
docs$text <- replace_white(docs$text)



## Markov model
buzz <- docs %>% pull(text)

markov_model <-
  generate_markovify_model(
    input_text = buzz,
    markov_state_size = 2L,
    max_overlap_total = 25,
    max_overlap_ratio = .85
  )


buzz.out <- markovify_text(
  markov_model = markov_model,
  maximum_sentence_length = NULL,
  output_column_name = "Gibberish",
  count = 30,
  tries = 200,
  only_distinct = TRUE,
  return_message = FALSE
)
```

``` r
kable(buzz.out, "html", align = "l", digits = 1) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F, position = "left")
```

<table class="table table-striped table-hover table-condensed" style="width: auto !important; ">
<thead>
<tr>
<th style="text-align:left;">
idRow
</th>
<th style="text-align:left;">
Gibberish
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
1
</td>
<td style="text-align:left;">
quantum computation that addresses computationally intensive training
</td>
</tr>
<tr>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
for example, the auto industry has seen the most widely adopted.
</td>
</tr>
<tr>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
these were followed closely by increases in automated information flows
in production by yearend
</td>
</tr>
<tr>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
data is fuel in the beginning, the it orgaadnization, they reorganized
it s probably unsurprising that unity created a skepticism around it.
many companies that are too complex and require sound governance at a
faster pace and reducing risk versus responding reactively to shortterm
fire drills and challenges in ml and dl algorithms grow too exhaustive
to perform customeragent sentiment analysis, and text processing,
</td>
</tr>
<tr>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
technologycentric approach identifies where there are no strangers to
discord. today, digital reality, and cloudnative development.
</td>
</tr>
<tr>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
these seamless experiences can be held accountadable for these use cases
</td>
</tr>
<tr>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
figure is an underappreciated fact, and it does so by communicating
requirements to developers, achieving a devsecops environment.
</td>
</tr>
<tr>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
your company? to answer this question, identify all the people that
voted against the same concept manifests in the other, humans currently
possess much of blockchain exploration, but the united states and is not
free. that said, the manufacturing floor creates a labeled, curated
dataset from which the computer learns to associate different shapes,
words, texts and sounds.
</td>
</tr>
<tr>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
for businesses looking to derive value from ai applications can help
finance work more efficiently and better than tell. try to control its
data and continuously refine. this process by using an artificial neural
net that is optimized to deploy new tools and functions that can be
difficult.
</td>
</tr>
<tr>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
very core of the enterprise s core, so it should lead the effort to
retool and rewire the future.
</td>
</tr>
<tr>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
blockchain technology is a question. is it prepared to work it must also
be able to tap cognitive insights can end up spending less time
exploring data and rigorous, ongoing training and a computing topology
in which cognitive computing and open to change, but actually getting
them out. our administration prosecuted more people calling, begging me,
don t consider a cloud migration. during planning, cios can calculate
project costs and increase switching costs, thus protecting the revenue
of the growing arvr maradket. because dr components are still challenges
to ai
</td>
</tr>
<tr>
<td style="text-align:left;">
12
</td>
<td style="text-align:left;">
to move digital assets between blockchains. think of that.
</td>
</tr>
<tr>
<td style="text-align:left;">
13
</td>
<td style="text-align:left;">
reengineering from the front lines
</td>
</tr>
<tr>
<td style="text-align:left;">
14
</td>
<td style="text-align:left;">
as figure shows, success breeds further success. the competitive gaps
and missed opportunity costs for laggards could be more of a broader
attack surface that can be able to reach . billion, while internet of
things iot applications is baked into systems during security
evaluations to try to correct a few years unveils new chips with double
the parts count of the most innovative companies in these regions are
moving toward largescale adoption in approximately one to come over.
</td>
</tr>
<tr>
<td style="text-align:left;">
15
</td>
<td style="text-align:left;">
there s just something magical about unbounded screen space, he says.
bottom line
</td>
</tr>
<tr>
<td style="text-align:left;">
16
</td>
<td style="text-align:left;">
though cios influence and prestige have grown markedly over the past
several years, relevant capabiliadties and concepts are immature, poorly
understood and unproven in missioncritical, atscale business operations.
this is done in our application rationalization and modernization
journey, we are seeing with digital platform delivering transformation
next steps contact us
</td>
</tr>
<tr>
<td style="text-align:left;">
17
</td>
<td style="text-align:left;">
given how entrenched traditional work, career, and hr models are,
reorganizing and reskilling workers around automation will likely share
the following technology layers can help make them more productive. you
can begin exploring them, the further projects get away from
paadperbased data delivery would make it possible to autoadmate and
accelerate into full production, the idea that the lack of existing apis
with internal and customerfacing privacy and data management,
governance, and controls frameadwork.
</td>
</tr>
<tr>
<td style="text-align:left;">
18
</td>
<td style="text-align:left;">
well, i ve understood a sentence properly? improving speech recognition
doesn t mean they can solve problems we cannot currently solve.
organizations that stand to benefit a few areas. first, maybe your
legacy data to adapt to a manageable collection of apis. the right time
solutions with the same basic mechanism can be put together a team to
help build predictive models for ml, deep learning algorithms are
typically assumed secure for approximately years or more aspects of your
platforms, don t forget the humans
</td>
</tr>
<tr>
<td style="text-align:left;">
19
</td>
<td style="text-align:left;">
comprehensive platform with windows server products.
</td>
</tr>
<tr>
<td style="text-align:left;">
20
</td>
<td style="text-align:left;">
the theme of this historic technology disruption is turning its
attention to these trends will reach exabytes per month in .
</td>
</tr>
<tr>
<td style="text-align:left;">
21
</td>
<td style="text-align:left;">
this document or an sap affiliate company. all rights reserved.
</td>
</tr>
<tr>
<td style="text-align:left;">
22
</td>
<td style="text-align:left;">
i also want to go through significant growing pains.
</td>
</tr>
<tr>
<td style="text-align:left;">
23
</td>
<td style="text-align:left;">
in particular, sap se or an sap affiliate company. all rights reserved.
</td>
</tr>
<tr>
<td style="text-align:left;">
24
</td>
<td style="text-align:left;">
deloitte, reinventing the erp space and a managing director
</td>
</tr>
<tr>
<td style="text-align:left;">
25
</td>
<td style="text-align:left;">
who is a strong social influence.
</td>
</tr>
<tr>
<td style="text-align:left;">
26
</td>
<td style="text-align:left;">
though cios influence and prestige have grown markedly over the next
five to years qc could have the capacity to carry the burden of solving
optimization problems, performing efficient simulations of quantum
mechanics
</td>
</tr>
<tr>
<td style="text-align:left;">
27
</td>
<td style="text-align:left;">
marketing insights helps marketers redirect their time from performing
analysis to taking targeted actions that previously required significant
development costs were about us million. this compares with gartner’s
estimated project cost of ownership for it.
</td>
</tr>
<tr>
<td style="text-align:left;">
28
</td>
<td style="text-align:left;">
interview with mark browning, vice president and chief product officer
at sysco. the systems that swiftly deal with the systems that serve the
relevant dataand questions that you are connecting two valadue networks
that may someadday make agi much more than three complaints and
questions at all levels continue to embrace change. our digital
innovation system, and enduser applications. each layer of the three
disciplines. in this research, dr. sajeed sees a future quantum maturity
versus reacting to it. over time, the notion of edge content delivery
has existed for many organizations will have a digital business services
</td>
</tr>
<tr>
<td style="text-align:left;">
29
</td>
<td style="text-align:left;">
james basden and michael cottrell, how utilities are using sap business
warehouse and new talent models
</td>
</tr>
<tr>
<td style="text-align:left;">
30
</td>
<td style="text-align:left;">
why technology, media, and tech execs say about ai
</td>
</tr>
</tbody>
</table>
