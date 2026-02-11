# gerbil-mcp for Gemini CLI

This repository, `gerbil-mcp`, provides "Gerbil Scheme language intelligence" that can extend the capabilities of the Gemini CLI. By integrating this project, Gemini can leverage tools and resources defined within `gerbil-mcp` to enhance its understanding and interaction with Gerbil Scheme code.

## Installation and Setup for Gemini CLI

To make `gerbil-mcp` available to your Gemini CLI, follow these steps:

1.  **Clone the repository:**
    If you haven't already, clone this repository to your local machine.

    ```bash
    git clone <repository-url>
    cd gerbil-mcp
    ```

2.  **Install Node.js dependencies:**
    Navigate to the project directory and install the necessary Node.js packages.

    ```bash
    npm install
    ```

3.  **Build the project:**
    Compile the TypeScript source code and prepare the project for use.

    ```bash
    npm run build
    ```

4.  **Configure Gemini CLI:**
    Run the `update` command from the `Makefile`. This command copies essential configuration files and skill definitions to your Gemini CLI's configuration directory, allowing Gemini to discover and utilize `gerbil-mcp`'s functionalities.

    ```bash
    make update
    ```

    This will copy files such as `CLAUDE.md`, `copilot-instructions.md`, and skill definitions to your `~/.claude/` directory, which the Gemini CLI uses for discovering external resources and skills.

## Usage

Once configured, the Gemini CLI will automatically have access to the intelligence provided by `gerbil-mcp`. You can then interact with Gemini as usual, and it will be able to apply its new understanding of Gerbil Scheme in relevant contexts.

For example, Gemini might be able to:
*   Answer questions about Gerbil Scheme syntax or idioms.
*   Help debug Gerbil Scheme code.
*   Generate or refactor Gerbil Scheme code snippets.
*   Utilize any specific tools or skills defined within the `gerbil-mcp` project (e.g., the `save-discoveries` skill).

Refer to the specific tools and resources within the `src/tools` and `src/resources` directories for a detailed understanding of what `gerbil-mcp` provides.
