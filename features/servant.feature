Feature: Servant

  Scenario Outline: Usage information
    When I run servant "<arg>"
    Then I should see usage information

    Examples:
      | arg    |
      | help   |
      | -h     |
      | --help |

  Scenario: Init
    When I run servant "init"
    Then I should see command output:
      """
      create servant
      create servant/tmp
      create servant/packages
      """
    And the directory "servant/tmp" should exist
    And the directory "servant/packages" should exist
    When I run servant "init"
    Then I should see command error:
      """
      Directory `servant` already exists.
      """

  Scenario: Shut up
    When I run servant "init"
    Then I should not see command output:
      """
      Loading vc-git...
      """

  Scenario Outline: Index
    When I run servant "init"
    And I run servant "<arg>"
    Then the file "servant/packages/archive-contents" should exist

    Examples:
      | arg     |
      | index   |
      | --index |

  Scenario: Path
    When I run servant "init --path tmp"
    Then the directory "tmp/servant/tmp" should exist
    And the directory "tmp/servant/packages" should exist
    When I run servant "index --path tmp"
    Then the file "tmp/servant/packages/archive-contents" should exist

  Scenario: Packages Path
    When I run servant "init"
    And I create directory "servant/awesome-packages"
    And I run servant "index --packages-path servant/awesome-packages"
    Then the file "servant/packages/archive-contents" should not exist
    But the file "servant/awesome-packages/archive-contents" should exist

  Scenario: Server
    Given I run servant "init"
    And I create file "servant/packages/foo-0.0.1.el" with contents:
      """
      ;;; foo.el --- Foo -*- lexical-binding: t; -*-

      ;; Version: 0.0.1

      (provide 'foo)

      ;;; foo.el ends here
      """
    When I run servant "start --index"
    Then requesting "/packages/foo-0.0.1.el" should respond with:
      """
      ;;; foo.el --- Foo -*- lexical-binding: t; -*-

      ;; Version: 0.0.1

      (provide 'foo)

      ;;; foo.el ends here
      """
    And requesting "/packages/archive-contents" should respond with:
      """

      (1 (foo . [(0 0 1) nil "Foo" single]))
      """
