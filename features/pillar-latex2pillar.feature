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

    When I insert "\cmindex{Stream}{readOnlyFileNamed:}"
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

  Scenario: Converting lists with item across lines
    When I clear the buffer
    When I insert "\begin{itemize}"
    And I insert a new line
    And I insert "    \item line1"
    And I insert a new line
    And I insert "line2"
    And I insert a new line
    And I insert "\end{itemize}"
    And I insert a new line

    Given I convert the buffer to latex
    Then I should see "-line1 line2"

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

  Scenario: Converting a figure
    When I clear the buffer
    And I insert "\begin{figure}[ht]"
    And I insert a new line
    And I insert "\centerline{\includegraphics[scale=0.5]{abcdeStef}}"
    And I insert a new line
    And I insert "\caption{A stream positioned at its beginning.}"
    And I insert a new line
    And I insert "\figlabel{abcde}"
    And I insert a new line
    And I insert "\vspace{.2in}"
    And I insert a new line
    And I insert "\end{figure}"
    And I insert a new line

    Given I convert the buffer to latex
    Then I should see "+A stream positioned at its beginning.>file://figures/abcdeStef.png|label=fig:abcde+"

  Scenario: Converting a code block
    When I clear the buffer
    And I insert "\begin{code}{@TEST |r|}"
    And I insert a new line
    And I insert "some code here"
    And I insert a new line
    And I insert "\end{code}"
    And I insert a new line

    Given I convert the buffer to latex
    Then I should see "[[["
    And I should see "some code here"
    And I should see "]]]"
    And I should not see "\begin"

  Scenario: Converting double-quotes
    When I clear the buffer
    And I insert "``foo'' ``bar''"

    Given I convert the buffer to latex
    Then I should see "''foo'' ''bar''"
