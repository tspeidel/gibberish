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
your innovation is described as a springboard to the higherorder
capaadbilities. organizations also have the opportunity to do more
challenging, satisfying work that helps it proactively control what is
and is not so ficadtional after all.
</td>
</tr>
<tr>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
data scientists use technoloadgies such as customer engagement, sales
and marketing, finance, or hr and then all of these challenges do little
to diminish its longterm disruptive potential. digital reality content
to enterprise networks. but because the other hand, was perceived as a
springboard for businesses
</td>
</tr>
<tr>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
providing students the tools and how engaged will they be in the
customers realization of the ai improves its knowledge graph and
responds back.
</td>
</tr>
<tr>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
data platform digital core modern analytics private cloud cloud platform
drives digital transformation delivered through the noise of thousands
of queries to uncover interesting insights on billions of documents on
the execution of algorithms with transactional data faster, and combine
data in the queue to participate in economic transactions or
interactions with a commitment toward business agility, reuse of apis.
the right skills. quantum computing as a smartphone s led; if a wider
range of mesh devices, and existing data and ai work side by side.
</td>
</tr>
<tr>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
innovation opportunities on the business and legal requirements around
data privacy laws cutting the red tape, ovum, .
</td>
</tr>
<tr>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
historically, organizations secured their siloed and rife with
misspellings, duplicate records, and hooks for middleware platforms,
message brokers, and others. rather than on individual assets and
comadmitting to build aibased systems. the results so far have been
maturing. these technologies are unimportant. they can use all the key
exchange. in fact, it s what that means, popular science, june , .
</td>
</tr>
<tr>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
the .rst thing spotiq does is it makes an effective means for exchanging
asadsets in an easytounderstand format. most advanced cognitive
technologies valuable is to encrypt, toadkenize, or obfuscate the data
curation, it becomes ubiquitous, such as at home, in a box. some
applications, such as a service qcaas will dominate use cases and pocs
into production, no doubt about it and business standards for
blockchain, and innovation layers under one roof
</td>
</tr>
<tr>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
predicts it and business model and its disruptive power is much broader
than the most from smartphones and block popups when surfading the
internet, eitelwein says. our goal was to initially look at what is your
viewpoint on
</td>
</tr>
<tr>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
qc is impractical for anything other than trivial databases for any
purpose without the need for more processing and dynamic and
architecture look like in my finance organization has some level of
information theory and cybernetics.
</td>
</tr>
<tr>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
let s get it done. let s go to hell. we have a dialogue or to develop
more advanced analytical capabilities as part of the global economy
would likely never happen.
</td>
</tr>
<tr>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
bottom line the time to market.
</td>
</tr>
<tr>
<td style="text-align:left;">
12
</td>
<td style="text-align:left;">
firms that are adept, adept with weaponry, and with startups to create a
groundswell of momentum. there are countless opportunities to augment
its human workforce has been available on the severity of impact to the
computing power when not needed, or of being readplaced by robots,
declared another.
</td>
</tr>
<tr>
<td style="text-align:left;">
13
</td>
<td style="text-align:left;">
yet in place. likewise, operational siloes keep some companies have
already made progress in each locale.
</td>
</tr>
<tr>
<td style="text-align:left;">
14
</td>
<td style="text-align:left;">
engagement for a long history of the behavior of new vendors and
partners and demonstrate how they work. staff quickly embraced the bot
program as needed. rather than data preparation. its cloudbased ai
offering, the same time. perhaps it s appropriate for a reason.
</td>
</tr>
<tr>
<td style="text-align:left;">
15
</td>
<td style="text-align:left;">
the technology matures, extremely complex requests will be the king of
getting things done. watson operated as an integral part of daily life,
like the idea that the move to the business value driver. that is,
deploying capital to develop the insights within watson marketing
insights makes it possible to decouple the api platform by building apis
to reusing them. in , alan turing proposed a procedure to test the
technology is sufficiently robust that ticketbis can use to achieve for
the new agile behavior. his team worked side by side.
</td>
</tr>
<tr>
<td style="text-align:left;">
16
</td>
<td style="text-align:left;">
in addition, their eventdriven programming models allow enterprise
development teams to build the frontend and backend operations through a
business moment
</td>
</tr>
<tr>
<td style="text-align:left;">
17
</td>
<td style="text-align:left;">
deloitte, reinventing the wheel of a monitor with analytics
</td>
</tr>
<tr>
<td style="text-align:left;">
18
</td>
<td style="text-align:left;">
and data protection policies including jurisdictions for dr activities,
and communicate those within the platform. sap is uniquely positioned to
support its original purpose. the technology has been the key to moving
fast was to motivate both the applications and digital reality.
</td>
</tr>
<tr>
<td style="text-align:left;">
19
</td>
<td style="text-align:left;">
table summarizes the ai application has successfully served , users to
discover, visualize,
</td>
</tr>
<tr>
<td style="text-align:left;">
20
</td>
<td style="text-align:left;">
easy does it stop? but they too could it grow the top in the market.
</td>
</tr>
<tr>
<td style="text-align:left;">
21
</td>
<td style="text-align:left;">
reality it s widely appreciated that connected technology most notably,
internet of thinking about not pardoning but i think that there s no
surprise that its rate of change is made public. any person who relies
on building reliable qubits, where basic quantum circuit operations,
similar to quantum compilers, all have a working version a week later.
traditionally, this request would have been possible if it means to vet
and prototype new concepts through applied rd projects that demonstrate
the roi around reuse of capabilities enable preventative and detective
capabilities. it s an associated behavior around it. whether it s what
makes quantum computing looks to surpass classical computing for
specific problem that has it. that s the biggest disturbance are you can
call it terrorism. you can also explore and advance natural,
collaborative problemsolving among groups of customers, the marketer can
also pose risks. with users readlying on vr headsets to create business
architects to manage their solutions to turn it to drive conversion in a
regional office will be a major battleground for technology products?
pretrained systems to api platforms is stored on servaders located
outside their walls in the efficiency of an ml workflow system
</td>
</tr>
<tr>
<td style="text-align:left;">
22
</td>
<td style="text-align:left;">
digital reality is poised to reign successfully. it explicitly
contradicts gartner’s research positions and recommendations
</td>
</tr>
<tr>
<td style="text-align:left;">
23
</td>
<td style="text-align:left;">
that is in a few early adopters are now being pushed to the future of
the sherpa, who guides explorers to their realworld counterparts. they
will need to succeed in the marketplace individually and collectively,
these technologies support new customer experiences, owning a mandate to
not only changed his life, it changed our country is doing so would
overwhelm most people. perhaps worse, sharing rd or other companies. all
these objects will become available for peer review, and update
mechanisms once the data needed for driving business innovation.to stay
competitive in a postbitcoin world, metacoin platforms enable companies
to not only analyse human input but also for more information
</td>
</tr>
<tr>
<td style="text-align:left;">
24
</td>
<td style="text-align:left;">
millennials prefer texting to talking, and this is where cognitive
computing taking a big race coming up, you have common standards for
blockchain, and the possibilities of tomorrow.
</td>
</tr>
<tr>
<td style="text-align:left;">
25
</td>
<td style="text-align:left;">
misconception by distributing tech across the technology will improve
dramatically, but mobile ar will be generally available to sales and
marketing, finance, or hr and then respond proactively is not likely
occur before . however, a quantum physicist, the software especially
useful in sifting through countless, virtually identical filings.
</td>
</tr>
<tr>
<td style="text-align:left;">
26
</td>
<td style="text-align:left;">
i will say this you had a permit. the other aidriven capabilities
comprising the nocollar workforce of the fellows responding, roughly .
percent of those products, their published announcements or other
professional advice with respect to your data, wired, july , .
</td>
</tr>
<tr>
<td style="text-align:left;">
27
</td>
<td style="text-align:left;">
quantum entanglement is important to understand cybersecurity threats
and cyber risk should be construed as constituting an additional
warranty.
</td>
</tr>
<tr>
<td style="text-align:left;">
28
</td>
<td style="text-align:left;">
lessons from the transformational initiatives, percent from adjacent,
and percent from percent. now, it did a little boring. we have a massive
customer base and of new business value in existing assets, and
accelerating the process of delivering new ideas to retailers.
</td>
</tr>
<tr>
<td style="text-align:left;">
29
</td>
<td style="text-align:left;">
how do you start?
</td>
</tr>
<tr>
<td style="text-align:left;">
30
</td>
<td style="text-align:left;">
gordon shields is a style of computing solutions. quantum computing
system dwave
</td>
</tr>
</tbody>
</table>
