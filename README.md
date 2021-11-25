## Set up

**Install**

* R
* RStudio
  * Not strictly necessary, but advisable
* Pandoc
  * Bundled with RStudio, but also available as a stand alone
* LateX
  * Simplest option is to install `Tinytex` package in R, but can be handled separately.

<hr />

**In RStudio**

Install bookdown

```{r}
install.packages("bookdown")
```

Install Tinytex

```{r}
install.packages("tinytex")

tinytex::install_tinytex()
```

## Template

| File                  | Description                                                  |
| --------------------- | ------------------------------------------------------------ |
| index.Rdm             | Contains file specific yaml, including document title, authorship, date etc. as well as a default welcome, copyright and conventions section. |
| _bookdown.yml         | Contains generic yaml, including book file name, output directory, cleaning instructions, language and certain UI elements, such as section breaks. |
| _output.yml           | Contains output specific yaml, generating html, pdf, and epub formats. |
| styles.css            | Global css settings.                                         |
| google_analytics.html | Copy and paste your google analytics snippet to this document. |

## Instructions

### Building the book

Open the file template.Rproj This will provide you with a workding directory in RStudio with all of the above files. 

#### styles.css

By default, contains the following two classes to build call out boxes:

* `note` for notes
* `advanced` for advanced concepts

#### Google Analytics

Set up (or request) a new project for your book and the code snippet with unique ID. Copy and paste this into your `google_analytics.html` file.

#### Update _output.yml

Update the toc before text to reflect the title of your book.

#### Update _bookdown.yml

Update the book_filename reflect the title of your book. Separate words with hyphens \"-\" and do not use special characters, including spaces.

#### Update index.Rdm

* In the yaml, adjust the title, description, and bibliography fields. If not using a bibliography, remove bibliography, biblio-style, and link-citations fields.
* Update the citation in the copyright section.
* Update or remove the conventions section.

#### Naming files

.Rmd / .md files should be numbered in the order in which you wish to have them compiled. index.Rmd will be compiled first by default and does not require numbering, subsequent files should look like:

* 01_FirstFile.Rmd
* 02_SecondFile.Rmd

#### Conventions

* Chapters are defined by h1 headers (#). This is default behaviour.
* Pagination is defined by h2 headers (##). This is defined in _bookdown.yml split_by: section
* Books may be further broken down into parts if needed using the special header # (PART). [See section 2.2.3 Specia Headers](https://bookdown.org/yihui/bookdown/markdown-extensions-by-bookdown.html#special-headers)
* bookdown files are exported to a <code>docs/</code> directory in the project root folder, defined in _bookdown.yml.

#### .nojekyll

GitHub pages uses Jekyll to render markdown to html. We're not using this, so to keep Jekyll out of the way, put a .nojekyll file in the <code>docs/</code> directory. To this, open a shell, navigate to your docs directory and execute <code>touch .nojekyll</code>

#### Avoiding errors

* If including citations, ensure that all bib records have a date field; it may be necessary to manually add \"n.d.\" if no date is available.
* When giving the book a name in _bookdown.yml file, ensure the file name is connected by hyphens \"-\", and that special charaters are avoided - this includes spaces.

### Publishing the book

* Create a repository for the project, upload content and set up git hub pages to look in the <code>docs/</code> directory.

