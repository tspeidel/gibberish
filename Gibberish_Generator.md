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

docs <- docs %>%
  mutate(text = strip(text, char.keep = c("?", ",", ";", ".", "-"))) %>%
  mutate(text = replace_contraction(text)) %>%
  mutate(text = replace_html(text)) %>%
  mutate(text = replace_non_ascii(text)) %>%
  mutate(text = replace_number(text, remove=TRUE)) %>%
  mutate(text = replace_symbol(text)) %>%
  mutate(text = replace_white(text)
)



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
quantum supremacy will be good outcomes, but it can often feel like
shooting at a disadvantage for whatever reason in the technology helps
ensure participants cannot cheat by misrepresenting what money or asset
they hold, and prevents them from spending the same speaker. the
alternative is the first step in the way they design, manuadfacture, and
deliver the desired business outcome.
</td>
</tr>
<tr>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
building new apis to the ecosystem, kan says. people underestimate how
big of an ai platform, as well as their respective logos are trademarks
of ibm or other publicly available sources, and accenture cannot confirm
the accuracy of customer profitability in the next two to six weeks in
length, and you can argue that this research
</td>
</tr>
<tr>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
the risks around data encryption and confidenadtiality are still very
far away from system centers and relocate them to the old lines become
blurred, thus creating a data strategy, these datasets will play an
important investment area for every data-mining function possible, even
when you can
</td>
</tr>
<tr>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
team members who think they have a more in-depth understanding of your
portfolio to address. also, when it comes to utilising chatbots beyond
simple research problem sets. unfortunately the error correction
overhead is significant requiring hundreds to thousands of predictive
models of qc and the type and amount of information and insights that
improve audience targeting, campaign optimization, and offer a
management team definitive, comprehensive counsel on, say, whether a
given set of subqueries to execute, and automatically makes
recommendations on sites like youtube are examples of digital
transformation?
</td>
</tr>
<tr>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
ai powered data modeling and advanced robotics soluadtions are currently
no overarchading technical standards for blockchain technologies, . open
source project. from an architectural viadsion to guide their actions.
scientists separated by oceans will convene in a decentralized approach
allows the marketer with superior ability to dynamically track material
flows, synchronize schedules, balance supply with demand, and drive
their established brick-and-mortar counterparts out of stock or a
developer who needs a robust finance system that can not deliver the
promise to transform theories and fresh ideas about the nature of the
consumer market, but kan sees the enterprise is a principal with
deloitte and touche llp s cyber risk services practice as well as help
them choose, and preview, thousands of lives for decades to break the
codes currently protecting networks and the sap analytics
</td>
</tr>
<tr>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
look, you take the form of a digital strategy has progressed. the
university of wales swansea, a master of science degree in psychology
from university of wales swansea, a master of business and technology
fit here the data challenges of today and how the agency engages with
citizens, seeking to drive business flexibility and reduces your options
or speed to market while lowering costs.
</td>
</tr>
<tr>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
illustration of blockchain, ai and security.
</td>
</tr>
<tr>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
manage it determine what roles are responsible for many organizations,
this is an opportunity to build and run through a central party. for
example, bitcoin. unlike cryptographic hash functions, which facilitates
change. to ease the complexity of automations, and the faster time to
go.
</td>
</tr>
<tr>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
so, thank you, because it is concealed. nobody would ever see what field
workers see as they started coming out were very gentle. he was very i
thought of, as soon as i said no way of bringing the corporate and
technology becomes an interdisciplinary task, with hr taking the
following terms and definitions are used to train the system.
</td>
</tr>
<tr>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
a lot of most conversational platforms
</td>
</tr>
<tr>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
vpas such as engaging customers and act delibaderately in the vote, let
s see how to advise patients on care advice and how engaged will they be
in our labs.
</td>
</tr>
<tr>
<td style="text-align:left;">
12
</td>
<td style="text-align:left;">
blockchain technologies and spatial, temporal, and social contributions
rather than using technology to operations, all underpinned by our
innovation efforts? how can a blockchain ledger, and no corruption of
past transactions is possible, it remains susceptible to fraud. watson
cognitive analytics are performed directly on native predictive models
for classes of real-world entities. the notion that a change that spans
the globe. additionally, as part of a chief digital officer or chief
algorithm officer demand that cios and enterprise architecture.
</td>
</tr>
<tr>
<td style="text-align:left;">
13
</td>
<td style="text-align:left;">
laissez-faire. decision-makers accepted that data without human
intervention, and therefore, it is not proven out at scale. ensure such
projects have put pressure on cios to be used merely to summarize
findings within a well-defined discipline after clearly understanding
all of the two top benefits of bots was a horrible, horrible inexcusable
thing.
</td>
</tr>
<tr>
<td style="text-align:left;">
14
</td>
<td style="text-align:left;">
innovation isn t and the way these families have suffered.
</td>
</tr>
<tr>
<td style="text-align:left;">
15
</td>
<td style="text-align:left;">
these insights are most important. this process by using streaming data
from its environment will often have an opportunity in all data
origination. companies must build the best driving route for an ai
strategy.
</td>
</tr>
<tr>
<td style="text-align:left;">
16
</td>
<td style="text-align:left;">
blockchain technologies will be virtualized
</td>
</tr>
<tr>
<td style="text-align:left;">
17
</td>
<td style="text-align:left;">
reality fair enough. currently, vr mobility is largely limited by
guarantee dttl , its network of nearly every sector to integrate
disparate data sources.
</td>
</tr>
<tr>
<td style="text-align:left;">
18
</td>
<td style="text-align:left;">
reorienting linear-thinking, quarterly revenue-focused stakeholders and
partners, enabling them to stop it.
</td>
</tr>
<tr>
<td style="text-align:left;">
19
</td>
<td style="text-align:left;">
unfortunately, without a governance system in which autoadmation,
analytics, real-time analysis and recommendations can help workers of
all agi component capabilities. we believe it s a global account manager
with accenture. she studied music education at the top challenge to
developing modern aradchitecture but to the most senior level in our
stores with multiple points of view every year. our thought provoking
research supported by proprietary data sets and will increasingly be
intelligent, rational and make it possible to dynamically manage
resources while integrating and delivering accurate data from across the
global spotlight.
</td>
</tr>
<tr>
<td style="text-align:left;">
20
</td>
<td style="text-align:left;">
deloitte consulting llp and serves as the digital natives typically
solve a problem into smaller chunks and crunching through large amounts
of business intelligence and i ve i have no obligation to you.
</td>
</tr>
<tr>
<td style="text-align:left;">
21
</td>
<td style="text-align:left;">
standardized technologies can increase the requirements not just for
giant companies but for the first sms was sent by vodafone engineer neil
papworth to colleague richard jarvis and simply read merry christmas .
</td>
</tr>
<tr>
<td style="text-align:left;">
22
</td>
<td style="text-align:left;">
peter diamandis, what are your competitors doing in this way, and its
possibilities.
</td>
</tr>
<tr>
<td style="text-align:left;">
23
</td>
<td style="text-align:left;">
how do we provide new tools and services to people’s individual
contexts. not every institution will build its own ip. this enables
complex decisions to derive immediate value
</td>
</tr>
<tr>
<td style="text-align:left;">
24
</td>
<td style="text-align:left;">
for the large strategic investments needed to develop and refine their
ani knowledge bases.
</td>
</tr>
<tr>
<td style="text-align:left;">
25
</td>
<td style="text-align:left;">
setting strategy and architecture leader
</td>
</tr>
<tr>
<td style="text-align:left;">
26
</td>
<td style="text-align:left;">
bill ruh, chief digital officer or chief analytics officer reinforced
the primacy of domain thinking.
</td>
</tr>
<tr>
<td style="text-align:left;">
27
</td>
<td style="text-align:left;">
governments typically intend to work on deadveloping post-quantum
encryption approach uses large volumes of electronic health records.
</td>
</tr>
<tr>
<td style="text-align:left;">
28
</td>
<td style="text-align:left;">
integrated circuits asics. broadly speaking, each of these initiatives
and the process of assessment and adaptive materials logistics processes
will be soon, millions of dollars in dollar bills, a lot of money in
north carolina, the great state of the groups and you have some examples
of this together to achieve instant value.
</td>
</tr>
<tr>
<td style="text-align:left;">
29
</td>
<td style="text-align:left;">
the api itself. gateways make it possible for users to stream their
selections directly to data outside of the package. the military is
afraid of them, but our military is afraid of them, but we re here a
little bit? it is a senior consultant with deloitte consulting llp and
specializes in information technology to operations, all underpinned by
the organization moves forward, iterate on the spot.
</td>
</tr>
<tr>
<td style="text-align:left;">
30
</td>
<td style="text-align:left;">
after completing a few early adopters at companies across industries,
several organizations have extended the problem those businesses that
drive them. does the enterprise to third party products, services and
represents risk in the morning and ask your favorite virtual assistant,
what is and is a scientific discipline that extracts information from
users without their consent. missteps by vendors by using and in the
popular vote. the popular media and among those predicting an ai
roadmap?
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
so what are essentially expert systems defined as algorithms that are
easily and freely accessible from cloudplatform.sap.com. second, you can
use
</td>
</tr>
<tr>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
figure . the implementation of a purchase order. some business events,
business moments and event
</td>
</tr>
<tr>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
as growing numbers of overextended cios are realizing, the traditional
computing architectures.
</td>
</tr>
<tr>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
nipun gupta is a human would need to be controversial, so i do not know
how to get percent of times articles. but jigsaw, a machine learning
capabilities of ai are in place to transform industry operating models.
funding in blockchain projects continues to grow in the sap analytics in
action
</td>
</tr>
<tr>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
lessons from the physical and virtual advisory for their online
channels. to realize its full potential, the vision behind them was, and
remains today, remarkably grounded. in the cloud by leveraging the
following day it looked like they had invested in erp implementations,
largescale custom systems, business process or externally as parts of
the state of its customers, which entails everything from sales to
availability to customer service department would benefit from ecosystem
success?
</td>
</tr>
<tr>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
build it and you see where companies are using cognitive tools that
users can access but not using quantum mechanics
</td>
</tr>
<tr>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
master data set or granularly explain differences between two parties
sharing information, typically via a mobile device, users can virtually
try on product shades using their photo or live video. meanadwhile,
guided virtual visits are poised to have in mind. but they used an agile
framework embracing devops rather than hinadder it. for example, how to
bring in new ways.
</td>
</tr>
<tr>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
automateddatadiscoveryencompassesaclassoftechnologiesthatautomatetheprocessofdata
analysis and reporting. though new core
</td>
</tr>
<tr>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
the performance and positively impacting revenue by analyzing instore
video feeds to determine whether an interaction to a quantumlevel
attack.
</td>
</tr>
<tr>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
international business machines corporation provides this publication
may be able to handle percent of respondents focused on digital
transformation?
</td>
</tr>
<tr>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
interview with brad fedosoff, vice president and head of arvr strategy,
unity technologies, october , .
</td>
</tr>
<tr>
<td style="text-align:left;">
12
</td>
<td style="text-align:left;">
are giving sloan valve integrated information at the edge
</td>
</tr>
<tr>
<td style="text-align:left;">
13
</td>
<td style="text-align:left;">
core integration. headgear manufacturers are designing apis that tie
core technologies and applications developed over the potential to run
quantum algorithms needed to solve the most significant adoption of
these processors included in most regions, the main barrier to adopting
ai. this lack of standardization in technology, business and are a new
digital industrial world by integrating its existing infrastrucadture.
however, technology integration and api strategy.
</td>
</tr>
<tr>
<td style="text-align:left;">
14
</td>
<td style="text-align:left;">
augmented reality ar and mixed reality.
</td>
</tr>
<tr>
<td style="text-align:left;">
15
</td>
<td style="text-align:left;">
this information is now aiming to preadvent companies from either
developing clear business plans around blockchain or dlt. leaders then
decided to devote his life to property and secret sauce embedadded in
decades of experience in business strategy upon which technology
augments human performance. managing both humans and machines in one
speed slow. it also requires a thorough inadventory of the stack has its
own set of all its past transactions as well.
</td>
</tr>
<tr>
<td style="text-align:left;">
16
</td>
<td style="text-align:left;">
how enterprise software and quantum encryption
</td>
</tr>
<tr>
<td style="text-align:left;">
17
</td>
<td style="text-align:left;">
john smith born on the api itself. gateways make it possible for users
to spend your time, and you had a lot of people did, a lot of money on
the market indicates strong investment in data and unlocking insights
often requires the introduction of sap s hanaae combines business
transactions and analytics paradigm there s nothing new here.
</td>
</tr>
<tr>
<td style="text-align:left;">
18
</td>
<td style="text-align:left;">
ibm ibm programs are provided an option for customers in the digital
transformation framework methodology
</td>
</tr>
<tr>
<td style="text-align:left;">
19
</td>
<td style="text-align:left;">
apis are implemented to identify and implement new processes and new,
compelling business models. digital business strategy and performance
</td>
</tr>
<tr>
<td style="text-align:left;">
20
</td>
<td style="text-align:left;">
learn from data. the process remains identical if the customer
</td>
</tr>
<tr>
<td style="text-align:left;">
21
</td>
<td style="text-align:left;">
in their companies. most respondents percent said it would have been put
in place the proper grouping and aggregation level to get a singular
view, observe trends and patterns. while human managers may well justify
the effort. even current ai capabilities the same result.
</td>
</tr>
<tr>
<td style="text-align:left;">
22
</td>
<td style="text-align:left;">
vertically suspended quantum computing looks to the marketer to specific
requirements of workers being replaced by persontoperson transactions
that involve thirdparty intermediaries such as data grows exponentially
in both management consulting and software tools, and potential biases.
large, comadplex projects have always felt he was very important process
to drive growth, innovation, and rewired industry ecosystems. often
overlooked, however, is their disruptive potential in core back and
midoffice systems and the particular possibilities they offer your
company. please allow us to perceive, interpret, and deduce on the
problem that could stymie knowledge sharing. for example, to serve as a
way that deadvelopers don t think superintelligent ai is coming down. i
wonder is it percent, matt, or percent? i would have had health care
too. think of this technology can aid companies with strategic
objectives over the country, and cities small and midsize partners
building valuable applications on sap cloud platform, lineofbusiness
managers can easily adapt and customize their it landscapes today. in
five edge use cases within capital maradkets. organizations should
understand how frontline managers see ai applications is understanding
why confidence in company data and are creating tremendous benefit, says
brad fedosoff, cibc vice president of business or partners.
</td>
</tr>
<tr>
<td style="text-align:left;">
23
</td>
<td style="text-align:left;">
as pioneering organizations may be made paradtially or fully autonomous.
by , ai can support global ecosystems, platform economies, complex
operational networks, and more. for example, where can i find talent who
can really blame veteran cios for harboring a few questions do you
fundamentally reimagine your business? how do i identify the most
significant agi breadcrumb appeared on january , .
</td>
</tr>
<tr>
<td style="text-align:left;">
24
</td>
<td style="text-align:left;">
however, we are organized. with more organizations modernizing it
operations can be detected more quickly and easily identify the
applications relative simplicity. when you do not control. we expect the
battle for smartphonebased ar to find cases with a user s realworld
environment. features body and motiontracking capabilities.
</td>
</tr>
<tr>
<td style="text-align:left;">
25
</td>
<td style="text-align:left;">
reengineering technology trend discussed earadlier in tech trends the
symphonic enterprise
</td>
</tr>
<tr>
<td style="text-align:left;">
26
</td>
<td style="text-align:left;">
the preceding sections speak to the business. the journey ahead.
</td>
</tr>
<tr>
<td style="text-align:left;">
27
</td>
<td style="text-align:left;">
doblin researchers examined companies in adjaadcent sectors pursuing
with their own bots according to gartner, chatbots will power of manled
interrogation and machineled cognitive recommendations and
personalization unique to applications into small, modular,
independently deployable services. this publication at any point in the
data represents, and the second amendment the most. we re changing it.
so we have a lot of really bad reasons.
</td>
</tr>
<tr>
<td style="text-align:left;">
28
</td>
<td style="text-align:left;">
every google search calls more than in manual approaches.
</td>
</tr>
<tr>
<td style="text-align:left;">
29
</td>
<td style="text-align:left;">
kieran norton is a design that consummates matches among providers and
consumers also referred to as edge computing.
</td>
</tr>
<tr>
<td style="text-align:left;">
30
</td>
<td style="text-align:left;">
a holistic embrace of the side effects of platforms include new entrants
and established players, including ibm, sap, oracle, tibco, mulesoft,
dell, software ag, ca, dell, and apigee.
</td>
</tr>
</tbody>
</table>
