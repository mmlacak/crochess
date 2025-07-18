# Copyright (c) 2024 Mario Mlačak, mmlacak@gmail.com
# Public Domain work, under CC0 1.0 Universal Public Domain Dedication. See LICENSING, COPYING files for details.


# Configuration file for the Sphinx documentation builder.
#
# This file only contains a selection of the most common options. For a full
# list see the documentation:
# https://www.sphinx-doc.org/en/master/usage/configuration.html

# -- Path setup --------------------------------------------------------------

# If extensions (or modules to document with autodoc) are in another directory,
# add these directories to sys.path here. If the directory is relative to the
# documentation root, use os.path.abspath to make it absolute, like shown here.
#
import os
import sys
# sys.path.insert(0, os.path.abspath('.'))


# -- Project information -----------------------------------------------------

project = 'Croatian Chess'
copyright = '2021, Mario Mlačak'
author = 'Mario Mlačak'

# The short X.Y version
version = "0.0.1.255:1434+20250718.061729"   # docs-new-lib-short-version-major-minor-feature-commit+meta~breaks-place-marker

# The full version, including alpha/beta/rc tags
release = "0.0.1.255:1434+20250718.061729"   # docs-new-lib-full-version-major-minor-feature-commit+meta~breaks-place-marker


# -- General configuration ---------------------------------------------------

# Add any Sphinx extension module names here, as strings. They can be
# extensions coming with Sphinx (named 'sphinx.ext.*') or your custom
# ones.
#
# extensions = [ ]
extensions = [  'sphinx.ext.autodoc',
                'sphinx.ext.autosectionlabel',
                'sphinx.ext.autosummary',
                'sphinx.ext.todo',
                'sphinx_rtd_theme',
]

# Add any paths that contain templates here, relative to this directory.
templates_path = ['_templates']

# The language for content autogenerated by Sphinx. Refer to documentation
# for a list of supported languages.
#
# This is also used if you do content translation via gettext catalogs.
# Usually you set "language" from the command line for these cases.
language = 'en-US'

# List of patterns, relative to source directory, that match files and
# directories to ignore when looking for source files.
# This pattern also affects html_static_path and html_extra_path.
exclude_patterns = [ '*IGNORE*', '*OLD*', '*NEW*' ]


# -- Options for sphinx.ext.autosectionlabel extension -----------------------

# https://www.sphinx-doc.org/en/master/usage/extensions/autosectionlabel.html

# True to prefix each section label with the name of the document it is in,
# followed by a colon. For example, index:Introduction for a section called
# Introduction that appears in document index.rst. Useful for avoiding
# ambiguity when the same section heading appears in different documents.
autosectionlabel_prefix_document = True

# If set, autosectionlabel chooses the sections for labeling by its depth.
# For example, when set 1 to autosectionlabel_maxdepth, labels are generated
# only for top level sections, and deeper sections are not labeled. It defaults
# to None (disabled).
# autosectionlabel_maxdepth = None

# -- Options for TODO extension ----------------------------------------------

todo_include_todos = True

# -- Options for HTML output -------------------------------------------------

# The theme to use for HTML and HTML Help pages.  See the documentation for
# a list of builtin themes.
#
html_theme = 'sphinx_rtd_theme' # 'classic' # 'nature' # 'bizstyle' # 'pyramid' # 'alabaster'

# Add any paths that contain custom static files (such as style sheets) here,
# relative to this directory. They are copied after the builtin static files,
# so a file named "default.css" will overwrite the builtin "default.css".
html_static_path = ['_static']

html_favicon = 'crochess.ico'


# # Thanks to buggy 'classic' theme (code colors are hard-coded, if specified
# # they are not being applied), it's not really possible to turn theme into
# # proper dark mode.
# #
# html_theme_options = {
#     # -- Options for HTML output, basic layout -----------------------------------
#     'body_max_width': 'none',
#     'navigation_with_keys': True,
#     'externalrefs': True,

#     # -- Options for HTML output, classic theme ----------------------------------
#     'footerbgcolor' : "#171717", # (CSS color): Background color for the footer line.
#     'footertextcolor' : "#FEFEFE", # (CSS color): Text color for the footer line.

#     'sidebarbgcolor' : "#202020", # (CSS color): Background color for the sidebar.
#     'sidebarbtncolor' : "#FF6E3B", # (CSS color): Background color for the sidebar collapse button (used when collapsiblesidebar is True).
#     'sidebartextcolor' : "#FEFEFE", # (CSS color): Text color for the sidebar.
#     'sidebarlinkcolor' : "#FF6E3B", # (CSS color): Link color for the sidebar.

#     'relbarbgcolor' : "#171717", # (CSS color): Background color for the relation bar.
#     'relbartextcolor' : "#FEFEFE", # (CSS color): Text color for the relation bar.
#     'relbarlinkcolor' : "#FF6E3B", # (CSS color): Link color for the relation bar.

#     'bgcolor' : "#303030", # (CSS color): Body background color.
#     'textcolor' : "#FEFEFE", # (CSS color): Body text color.
#     'linkcolor' : "#FF6E3B", # (CSS color): Body link color.
#     'visitedlinkcolor' : "#b34d29", # (CSS color): Body color for visited links.

#     'headbgcolor' : "#272727", # (CSS color): Background color for headings.
#     'headtextcolor' : "#FEFEFE", # (CSS color): Text color for headings.
#     'headlinkcolor' : "#FF6E3B", # (CSS color): Link color for headings.

#     # This doesn't work.
#     # 'codebgcolor' : "#303030", # (CSS color): Background color for code blocks.
#     # 'codetextcolor' : "#FEFEFE", # (CSS color): Default text color for code blocks, if not set differently by the highlighting style.

#     'bodyfont' : "DejaVu Sans,Verdana,sans-serif", # (CSS font-family): Font for normal text.
#     'headfont' : "DejaVu Sans,Verdana,sans-serif", # (CSS font-family): Font for headings.
# }

# # Light-grey 'classic' theme.
# #
# html_theme_options = {
#     # -- Options for HTML output, basic layout -----------------------------------
#     'body_max_width': 'none',
#     'navigation_with_keys': True,
#     'externalrefs': True,
#
#     # -- Options for HTML output, classic theme ----------------------------------
#     'footerbgcolor' : "Gray", # (CSS color): Background color for the footer line.
#     'footertextcolor' : "NavajoWhite", # (CSS color): Text color for the footer line.
#
#     'sidebarbgcolor' : "DarkGray", # (CSS color): Background color for the sidebar.
#     'sidebarbtncolor' : "SaddleBrown", # (CSS color): Background color for the sidebar collapse button (used when collapsiblesidebar is True).
#     'sidebartextcolor' : "Black", # (CSS color): Text color for the sidebar.
#     'sidebarlinkcolor' : "SaddleBrown", # (CSS color): Link color for the sidebar.
#
#     'relbarbgcolor' : "DimGray", # (CSS color): Background color for the relation bar.
#     'relbartextcolor' : "Black", # (CSS color): Text color for the relation bar.
#     'relbarlinkcolor' : "NavajoWhite", # (CSS color): Link color for the relation bar.
#
#     'bgcolor' : "Silver", # (CSS color): Body background color.
#     'textcolor' : "Black", # (CSS color): Body text color.
#     'linkcolor' : "SaddleBrown", # (CSS color): Body link color.
#     'visitedlinkcolor' : "IndianRed", # (CSS color): Body color for visited links.
#
#     'headbgcolor' : "LightGray", # (CSS color): Background color for headings.
#     'headtextcolor' : "Black", # (CSS color): Text color for headings.
#     'headlinkcolor' : "IndianRed", # (CSS color): Link color for headings.
#
#     # This doesn't work.
#     'codebgcolor' : "LightGray", # (CSS color): Background color for code blocks.
#     'codetextcolor' : "Black", # (CSS color): Default text color for code blocks, if not set differently by the highlighting style.
#
#     'bodyfont' : "DejaVu Sans,Verdana,sans-serif", # (CSS font-family): Font for normal text.
#     'headfont' : "DejaVu Sans,Verdana,sans-serif", # (CSS font-family): Font for headings.
# }

# Read-the-docs theme.
#
html_theme_options = {
    # 'analytics_id': 'G-XXXXXXXXXX',  #  Provided by Google in your dashboard
    # 'analytics_anonymize_ip': True,
    'logo_only': False,
    'prev_next_buttons_location': 'bottom',
    'style_external_links': True,
    'vcs_pageview_mode': '',
    'style_nav_header_background': 'LightGray',
    'flyout_display': 'hidden',
    'version_selector': True,
    'language_selector': True,
    # Toc options
    'collapse_navigation': True,
    'sticky_navigation': True,
    'navigation_depth': 9,
    'includehidden': True,
    'titles_only': False,
}
