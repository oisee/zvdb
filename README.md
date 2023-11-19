# ABAP Vector Search Library

The ABAP Vector Search Library is a Vector Database developed entirely in ABAP, designed to offer an independent solution without relying on external vector databases. It's particularly tailored for the "chat with your documents" scenario, enabling the slicing of documents into manageable chunks and the injection of these semantically relevant segments into user prompts. This approach facilitates an interactive and efficient method for handling and responding to user queries within document-based systems.

The library employs the RAG (Retrieval Augmented Generation) technique, which is integral to its functionality, enhancing the interaction between users and document data.

RAG is the technique that is powering the “chat with your documents” scenario:
 - Slice documents into chunks
 - Inject semantically relevant chunks into the Prompt:
```code
Considering only information below:
###
{chunk01}
{chunk02}
{chunk03}
###
please provide response to the following request: 
{query from user}
```

## Installation

Install this repository using [abapGit](https://github.com/abapGit/abapGit#abapgit).

© 2023 Alice Vinogradova.  
For more information, see the [LICENSE](LICENSE) file in this repository.
