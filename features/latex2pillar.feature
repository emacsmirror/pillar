@latex2pillar
Feature: LaTeX to Pillar
  In order leverage Pillar for LaTeX books
  As an author using Pillar
  I want to convert LaTeX files to Pillar syntax easily

  Background:
    Given I am in buffer "foo.pillar"
    And I clear the buffer
    And I turn on pillar-mode
    And I load latex2pillar

  Scenario: The buffer is cleaned
    When I clear the buffer
    When I insert "% foo bar"
    And I convert the buffer to latex
    Then buffer should be empty

    When I insert "\dc{foo bar}"
    And I convert the buffer to latex
    Then buffer should be empty

    When I insert "\clsindexmain{Stream}"
    And I convert the buffer to latex
    Then buffer should be empty

    When I insert "\needlines{Stream}"
    And I convert the buffer to latex
    Then buffer should be empty

  Scenario: Converting itemize lists
    When I clear the buffer
    When I insert "\begin{itemize}"
    And I insert a new line
    And I insert "\item Foo"
    And I insert a new line
    And I insert "\item Bar"
    And I insert a new line
    And I insert "\end{itemize}"
    And I insert a new line

    Given I convert the buffer to latex
    Then I should see "-Foo"
    And I should see "-Bar"
    And I should not see "itemize"

  Scenario: Converting lists with whitespace
    When I clear the buffer
    When I insert "\begin{itemize}"
    And I insert a new line
    And I insert "    \item Foo"
    And I insert a new line
    And I insert "  \item Bar"
    And I insert a new line
    And I insert "\end{itemize}"
    And I insert a new line

    Given I convert the buffer to latex
    Then I should see "-Foo"
    When I place the cursor before "-Foo"
    Then The cursor should be at point "1"
    When I place the cursor before "-Bar"
    Then The cursor should be at point "6"

  Scenario: Converting lists with multiple \item per line
    When I clear the buffer
    When I insert "\begin{itemize}"
    And I insert a new line
    And I insert "    \item Foo\item Bar"
    And I insert a new line
    And I insert "\end{itemize}"
    And I insert a new line

    Given I convert the buffer to latex
    Then I should see "-Foo"
    When I place the cursor before "-Foo"
    Then The cursor should be at point "1"
    When I place the cursor before "-Bar"
    Then The cursor should be at point "6"

  Scenario: Converting enumerate lists
    When I clear the buffer
    When I insert "\begin{enumerate}"
    And I insert a new line
    And I insert "\item Foo"
    And I insert a new line
    And I insert "\item Bar"
    And I insert a new line
    And I insert "\end{enumerate}"
    And I insert a new line

    Given I convert the buffer to latex
    Then I should see "-Foo"
    And I should see "-Bar"
    And I should not see "enumerate"

  Scenario: Converting description lists
    When I clear the buffer
    When I insert "\begin{description}"
    And I insert a new line
    And I insert "\item[   Foo1]Foo2"
    And I insert a new line
    And I insert "\item[Bar1   ]  Bar2"
    And I insert a new line
    And I insert "\end{description}"
    And I insert a new line

    Given I convert the buffer to latex
    Then I should see ";Foo1"
    And I should see ":Foo2"
    And I should see ";Bar1"
    And I should see ":Bar2"
    And I should not see "description"

  Scenario: Sectioning commands are replaced
    When I clear the buffer
    When I insert "\chapter{Title}"
    And I convert the buffer to latex
    Then I should see "!Title"

    When I clear the buffer
    When I insert "\section{Title}"
    And I convert the buffer to latex
    Then I should see "!!Title"

    When I clear the buffer
    When I insert "\paragraph{Title}"
    And I convert the buffer to latex
    Then I should see "!!!!!Title"

  Scenario: Converting 0-arg commands
    When I clear the buffer
    When I insert "\ie"
    And I convert the buffer to latex
    Then I should see "''i.e.'',"

    When I clear the buffer
    When I insert "\ie{}"
    And I convert the buffer to latex
    Then I should see "''i.e.'',"
    And I should not see "{}"

  Scenario: Converting 1-arg commands
    When I clear the buffer
    When I insert "\clsind{FileStream}"
    And I convert the buffer to latex
    Then I should see "==FileStream=="

  Scenario: Converting 2-arg commands
    When I clear the buffer
    When I insert "\mthind{FileStream}{binary}"
    And I convert the buffer to latex
    Then I should see "==FileStream>>binary=="


