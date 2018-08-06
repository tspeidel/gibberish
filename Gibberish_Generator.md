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
This has led firms like Gartner to publish the now popular ???[Hype
Cycle](https://en.wikipedia.org/wiki/Hype_cycle)???.

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
become agile by extending your current data capacity, efficiency or lack
thereof, and costs, and redeploy employees to make sure, when i make a
statement i like to be having any form of weaponry. we have the luxury
of being readplaced by robots, declared another.
</td>
</tr>
<tr>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
since , beachbody, a provider of fitness, nuadtrition, and weightloss
programs, has offered cusadtomers a wide range of intents. the nlu
interprets enduser intents and provides only eventual consistency.
process designers, architects and developers more leeway to focus on
erasing the boundaradies between macro it domains such as architecture
and enabling technologies. in the healthcare sector, for example, public
and private sector guy. for whatever reason decided not to construct
centralized, enterprisewide controls and governors rather, it is
important because it is very rare that you will teach. from the company
s data platform digital core modern analytics private cloud cloud
platform
</td>
</tr>
<tr>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
machine learning and contextual user experiences for customers and
faster user experience.they give people the ability to transact
</td>
</tr>
<tr>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
vodafone germany coupled the infrastructure or what time is now under
the obama plans. one of the academic year. students responded in
different time horizons with first and foremost on inventory visibility.
when purchasing an item online, a customer or traveling.
</td>
</tr>
<tr>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
the key drivers of platformbased businesses. over time, more
conversational platforms
</td>
</tr>
<tr>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
nipun gupta is a perfect solution to interoperability, but we have tough
people. strong people. they are not introducing disorder you are
connecting two valadue networks that may affect your finances or your
business.
</td>
</tr>
<tr>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
what roles do recent technology innovations like machine learning
algorithm.
</td>
</tr>
<tr>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
allianz se, insurers and reinsurers launch blockchain initiative b i,
allianz is working to strike the right thing to have you had a puddle on
your business rather professional, and managed on a clear understanding
of ai that provides systems with a dynamic data management solutions
take you?
</td>
</tr>
<tr>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
start a new class of citizen data scientists to produce the desired
outcome.spam .ltering is a classic example of a sudden we start and that
wasn t even know how many bags each shopper is carrying.
</td>
</tr>
<tr>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
tmt leaders see cognitive technology categorized by naturallanguage
processing nlp, is a tough group of cuttingedge competitors, even early
adopters recognize a cat in a quantum cryptography solutions available
during the next couple of months. we believed that modernizaadtion of
technology trends, challenges, and identify critical areas for applying
ai to succeed are forcing supply chain processes. this can be leveraged
for data veracity, spacex uses a single shared ledger technology
implications of blockchain is evolving from a digital strategy in higher
education institutions.
</td>
</tr>
<tr>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
back and flesh out traditional business models matter for cios
</td>
</tr>
<tr>
<td style="text-align:left;">
12
</td>
<td style="text-align:left;">
examples of compound lens insights n customers are at the core elements
of the hacker industry and its values. in short people are inherently
cowards. if they are free to try and understand their it stacks and
organizations continue to impede information sharing.
</td>
</tr>
<tr>
<td style="text-align:left;">
13
</td>
<td style="text-align:left;">
while usage is still hope. these days, most enterprises are becoming
more pressing.
</td>
</tr>
<tr>
<td style="text-align:left;">
14
</td>
<td style="text-align:left;">
he realized that to become widespread. for that to become distracted or
discouradaged. you may have to show your spirit. it is essential to the
tactical, daytoday comfort zone of structured, businessoriented
questions such as imadages, audio, video, and the associated builtin ai
functionality, grow from metadata structures and models of buildings,
cities and other corporate functions.
</td>
</tr>
<tr>
<td style="text-align:left;">
15
</td>
<td style="text-align:left;">
inventory, classify, and maintain thousands of queries to uncover
interesting insights on the problem that could be consumed in broad and
expanding internationally these shared resources became shared
obstacles, compromising speed.
</td>
</tr>
<tr>
<td style="text-align:left;">
16
</td>
<td style="text-align:left;">
to create a foundation with greenfield solutions can help you to easily
analyze billions of rows of dataacross multiple sources all with virtual
reality. while we were speaking. this event just happened. in fact,
nearly of students prefer messaging over any other claims related to
these advances.
</td>
</tr>
<tr>
<td style="text-align:left;">
17
</td>
<td style="text-align:left;">
blockchain technologies provide an always on connection to office are
likely exploring other digital entities for people who are
disproportionately affected in employment and income; advocate for and
responding faster, more consistent, and agile governance mechanisms both
for the cio agenda a higher level of seradvice to that end,
organizations can consume the latest blockchain trend
</td>
</tr>
<tr>
<td style="text-align:left;">
18
</td>
<td style="text-align:left;">
technologycentric approach to blockchain development resources on
defenses such as
</td>
</tr>
<tr>
<td style="text-align:left;">
19
</td>
<td style="text-align:left;">
ai applications in every geography. the greatest value, and developing
operational capabilities within it. by making the statement when i make
a plan. map out a transformation plan for future student enrolments.
</td>
</tr>
<tr>
<td style="text-align:left;">
20
</td>
<td style="text-align:left;">
it s operations and analysis within health sciences that uses
encryption. for example, microsoft office and they won t do it.
</td>
</tr>
<tr>
<td style="text-align:left;">
21
</td>
<td style="text-align:left;">
with this technology can effectively upskill your agents, streamline how
you are trying to solve. gartner estimates that
quantumcomputingasaservice will comprise nearly half of the other
approaches for context, speed, and agility.
</td>
</tr>
<tr>
<td style="text-align:left;">
22
</td>
<td style="text-align:left;">
the next decade, advances in one step can result in larger encrypted
files and sigadnature sizes. another, more straightforward postquantum
encryption approach uses large symmetadric keys. symmetric keys, though,
require some way to industrializaadtion. for example, medical product
manufacturer
</td>
</tr>
<tr>
<td style="text-align:left;">
23
</td>
<td style="text-align:left;">
there s math to prove themselves in data literacy. both small startups
and large vendors making big bets until you know the students, and
frankly for whatever reason, i always felt he was terrible on thefive
yesterday. angry and obnoxious, she will never happen. the democrats and
they stick together. they do. they said, you need the electoral college,
which, by the platform business model and its competitive advantages
</td>
</tr>
<tr>
<td style="text-align:left;">
24
</td>
<td style="text-align:left;">
lessons from the start. glorioso s team chose a platform business model,
such an ecosystem of providers to outline how they and you can better
set priorities, manage relaadtionships, and juggle responsibilities.
moreover, this leadership framework may even inspire some constructive
soulsearching into how you interact with their studies. for example,
teams in beachbody s leadership team to pursue any course of action
</td>
</tr>
<tr>
<td style="text-align:left;">
25
</td>
<td style="text-align:left;">
theresa morelli is a strong social influence.
</td>
</tr>
<tr>
<td style="text-align:left;">
26
</td>
<td style="text-align:left;">
this formula will be captured is often called instant visual
translation.
</td>
</tr>
<tr>
<td style="text-align:left;">
27
</td>
<td style="text-align:left;">
explore new redbooksae publications, residencies, and workshops with the
decision, cognitive insights can help make them visible to students and
engages them is the ecosystem workforce design for optimization
problems; toq, a highlevel language translator used for fraud detection
and response rates to marketing campaigns.
</td>
</tr>
<tr>
<td style="text-align:left;">
28
</td>
<td style="text-align:left;">
to sap s hana, a realtime erp suite built on sap cloud platform
</td>
</tr>
<tr>
<td style="text-align:left;">
29
</td>
<td style="text-align:left;">
dwave users can access but not always easy. in tradiadtional supply
chains, datasets are often used as a source of truth? with security and
risk management. he serves as the capital of israel. everybody. for many
organizations, this is a key advantage of innovations past. some of the
publication. ibm may make the decision engine model makes a service
providers will begin modernizing their approaches to both improved
customer service interactions by the centralized platform business model
and how to achieve a reliable implementation of quantum computing from
academic theory to the brand.
</td>
</tr>
<tr>
<td style="text-align:left;">
30
</td>
<td style="text-align:left;">
for finance organizations, the digital ecosystem opportunity and
potential challenge exponential innovations such as reputation services,
payment services, warranty services and other revolutionary technologies
to generate new interactions between humans and machines are similar
applying new tactics, their ultimate goal of blockchain technology is
the use cases emerging around it. many companies are proving grounds for
ai
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

\`\`\`
