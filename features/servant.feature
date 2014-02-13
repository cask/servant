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

