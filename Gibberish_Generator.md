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
mitch derman for your function, focusing first on bringing new
capabilities to extend the existing linear process control and certainty
are paramount.
</td>
</tr>
<tr>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
. billion investment in time to value. by uniting the business
operations formerly cio of network and computing power. moreover, larger
keys often result in a virtual meeting with students as a common library
of algorithms will continue to expand beyond product engineers and don t
want people coming into this country based on realtime visibility into
trends across funcadtions and geographies, or by third parties.
</td>
</tr>
<tr>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
how do i develop an architectural standpoint, beachbody built the
ondemand platform in just eight weeks, the team recognized that the
market today. ibm s deep blue beat reigning world champion, garry
kasparov, in a dynamic, interadconnected new core
</td>
</tr>
<tr>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
immediate insight for artificial intelligence where you take a look at
what he said he was very obnoxious. it was a percent efficiency. the
potential of this year s tech trends the symphonic enterprise, an idea
that describes strategy, technology, and working sets for basic data
management solutions take you?
</td>
</tr>
<tr>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
businesses that drive standards within a particular business problem.
routh realized that the queries must be clearly communicated and easily
apply insights to anybusinessperson. withspotiq,younowhavethepower of a
telephone voice response system, albeit one that can handle unusually
large and will grow to us .
</td>
</tr>
<tr>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
and innovate at your own risk.
</td>
</tr>
<tr>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
push for standardization in technology, business and governmental
challenges. in reality, trying to say, why is superposition important?
superposition of states is what I would call it. because there s a fine
man, and he went this way also enables a quantum algorithm and computing
infrastructure to code. it is able to use an approach that distributes
functionality to the next few years.
</td>
</tr>
<tr>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
anthony abbatiello is a volume of data that is populated to allow the
user experience of visiting their highend stores for their support.
likewise, create mechanisms for gauging api consumption is arguably more
important than ever on what would you say what happens is you fight
through the mesh will extend immersive applications beyond financial
services, health care, unfortunately, john mccain why he did was a group
on one side that was always a big data margin assurance solution powered
by sap se or an sap affiliate company. all rights reserved.
</td>
</tr>
<tr>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
international business machines corporation provides this publication
contains general information only, and none of deloitte s enterprise
platforms offering where he helps clients transform their business.
</td>
</tr>
<tr>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
organizations seeking strategic advantage through early technology
adoption find that some people found funny. if they are released.
</td>
</tr>
<tr>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
digital reality dr an umbrella term for all customers. see the
competitive advantage for platforms is stored personal information, as
well as gaining insights from existing datasets. ml can help companies
extract more value from their disparate remote locations, which could be
used on roadways in limited, welldefined, geofenced and controlled
environments. advances in machine intelligence and i can t predict the
likelihood of a customer migrating from a drawerful of different
documents, a swivel chair task that a strong foundation for the
technology companies with the same digital backbone needed for
highthroughput computing needs for advanced simulation, operations and
manufacturing, and supply organizations deploy some digital form, and if
the customer
</td>
</tr>
<tr>
<td style="text-align:left;">
12
</td>
<td style="text-align:left;">
follow us on facebook
</td>
</tr>
<tr>
<td style="text-align:left;">
13
</td>
<td style="text-align:left;">
simplification is part of their credibility continues to lie in
maintaining missioncritical processes
</td>
</tr>
<tr>
<td style="text-align:left;">
14
</td>
<td style="text-align:left;">
the institutes has supported the creation of computeraided design
methodologies, companies are recognizing they must be ready for change.
its business model should be freely accessible from
cloudplatform.sap.com. second, you can extend or build their own four
walls. in today s habits of success. even though this approach is more
apt to consider designing quantumspecialized algorithms to solve the
right decision in the near future, hr will manage automated workers by
designing governance and processes of standing up, building on sap cloud
platform
</td>
</tr>
<tr>
<td style="text-align:left;">
15
</td>
<td style="text-align:left;">
andy mills, virtual reality vr, augmented reality and mixed reality
practice. he has more than doubling every to months. just a formalized
way to make some operating decisions.
</td>
</tr>
<tr>
<td style="text-align:left;">
16
</td>
<td style="text-align:left;">
naturallanguage understanding nlu capabilities.
</td>
</tr>
<tr>
<td style="text-align:left;">
17
</td>
<td style="text-align:left;">
over the potential to improve student completions. so far, ivy tech
community college initiative to collect and visualize the surroundings
and the nocollar trend a viable option for exposing and monadetizing
intellectual property. these new transformational technologies.
</td>
</tr>
<tr>
<td style="text-align:left;">
18
</td>
<td style="text-align:left;">
as with many vr displays requires at least significantly weaken
cryptography. if true, this would be able to adjust contrast, color and
size was more than digit number sequences that comprise popular
encryption protocols like rsa or diffiehelladman. mature quantum
computers reach qubits or more, depending on the future behaviors with
more participants and more decentralized approaches such as identity and
authentication in the world to collaborate and transact. hence,
customers stick with a wide variety of digital transformation journey
</td>
</tr>
<tr>
<td style="text-align:left;">
19
</td>
<td style="text-align:left;">
mbank is poland s largest shipping port, rotterdam, has launched a
secadond pilot in the meantime, private and publicsector technology
companies. he is a common misconception in the cloud. in this kind of
stress that she will never make it on a daily feed of insights no longer
relegated to pure science .ction, as often depicted by many hollywood
.lms. since those early days see hype cycle for the customer experience
that delivers personalization through automation.
</td>
</tr>
<tr>
<td style="text-align:left;">
20
</td>
<td style="text-align:left;">
in , a group of people, and we are seeing cios enlist film and videogame
design veterans with computergenerated image cgi exadpertise to help
guide you to look at what is your viewpoint on
</td>
</tr>
<tr>
<td style="text-align:left;">
21
</td>
<td style="text-align:left;">
the reengineering trend is a good enough nearsolution quickly is also
the lead in digital transformation journey. we took a bold stance to
reevaluate a techadnology transformation initiative that was built from
the front lines
</td>
</tr>
<tr>
<td style="text-align:left;">
22
</td>
<td style="text-align:left;">
using a computer to process and freeform innovation for developing these
recommendation engines. in , microsoft announced bot builder. the
framework must provide this in the coming years, and retired, people in
that timeframe, and cycle time of this research, dr. sajeed cautions
that the real story was not an easy task. analyzing customer data
</td>
</tr>
<tr>
<td style="text-align:left;">
23
</td>
<td style="text-align:left;">
deloitte touche llp and has accrued more than just a few tightly scoped
projects can then leverage other technologies in the quadrant of
smartphone ai use cases
</td>
</tr>
<tr>
<td style="text-align:left;">
24
</td>
<td style="text-align:left;">
in figure . platform business model should be incremental.
</td>
</tr>
<tr>
<td style="text-align:left;">
25
</td>
<td style="text-align:left;">
detailed insights and solutions in the leading use cases that technology
assets than by controlling them. embracing this trend fully will require
a significant data science programs for clients in more than equipment
decisions, comprising policy and risk governance implementation, cyber
threat monitoring, vulnerability management, identity and asset
provenance
</td>
</tr>
<tr>
<td style="text-align:left;">
26
</td>
<td style="text-align:left;">
managing your portfolio to address. also, when you deal with them.
without a central notion in the entertainment and gaming inaddustries
ramp up digital reality isn t likely to play a crucial customer may be
changed by sap using sap cloud platform
</td>
</tr>
<tr>
<td style="text-align:left;">
27
</td>
<td style="text-align:left;">
gartner describes this capability funds much of it are dedicated to
remaining tech fluent and staying on top of blockchain technology
itself. blockchain technology itself. blockchain technology is poised to
advance far beyond gaming looking to digital enlightadenment, it is
useful to correct a few misconceptions that digital is as much as
percent and improved with quantum computing. despite claims to have you
back from delivering more value from it concern to business s top
initiative priorities, can help mitigate some quantum risk. also, seek
opportuadnities to collaborate over a period of time. first, you can
</td>
</tr>
<tr>
<td style="text-align:left;">
28
</td>
<td style="text-align:left;">
ibmae watson assistant delivers scalable, engaging experiences across
all other product and service will incorporate more eventdriven
approaches across their product lines. examples include
</td>
</tr>
<tr>
<td style="text-align:left;">
29
</td>
<td style="text-align:left;">
swarms will help united ensure that sap cloud platform
</td>
</tr>
<tr>
<td style="text-align:left;">
30
</td>
<td style="text-align:left;">
simplification is part of this in this case, these two features weaken
the competitive advantages of the future of work. only percent of
companies inadnovation investments with their dr initiatives? finally,
your supplier, vendors, and suppliers as well as the gdpr s provisions
for data cleansing processes that had evolved to supadport largescale
packaged software configuration to experience insights in watson
marketing insights tenant is provisioned, the architecture design.
</td>
</tr>
</tbody>
</table>
