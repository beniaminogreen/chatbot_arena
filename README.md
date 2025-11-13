---
dataset_info:
  features:
  - name: id
    dtype: string
  - name: model_a
    dtype: string
  - name: model_b
    dtype: string
  - name: winner
    dtype: string
  - name: evaluation_session_id
    dtype: string
  - name: evaluation_order
    dtype: int32
  - name: conversation_a
    list:
    - name: role
      dtype: string
    - name: content
      list:
      - name: type
        dtype: string
      - name: text
        dtype: string
      - name: image
        dtype: string
      - name: mimeType
        dtype: string
  - name: conversation_b
    list:
    - name: role
      dtype: string
    - name: content
      list:
      - name: type
        dtype: string
      - name: text
        dtype: string
      - name: image
        dtype: string
      - name: mimeType
        dtype: string
    - name: num_tokens
      dtype: int32
  - name: full_conversation
    list:
    - name: user
      struct:
      - name: role
        dtype: string
      - name: content
        list:
        - name: type
          dtype: string
        - name: text
          dtype: string
        - name: image
          dtype: string
        - name: mimeType
          dtype: string
    - name: model_side_a
      struct:
      - name: role
        dtype: string
      - name: content
        list:
        - name: type
          dtype: string
        - name: text
          dtype: string
        - name: image
          dtype: string
        - name: mimeType
          dtype: string
    - name: model_side_b
      struct:
      - name: role
        dtype: string
      - name: content
        list:
        - name: type
          dtype: string
        - name: text
          dtype: string
        - name: image
          dtype: string
        - name: mimeType
          dtype: string
  - name: conv_metadata
    struct:
    - name: sum_assistant_a_tokens
      dtype: int32
    - name: header_count_a
      struct:
      - name: h1
        dtype: int64
      - name: h2
        dtype: int64
      - name: h3
        dtype: int64
      - name: h4
        dtype: int64
      - name: h5
        dtype: int64
      - name: h6
        dtype: int64
    - name: list_count_a
      struct:
      - name: ordered
        dtype: int64
      - name: unordered
        dtype: int64
    - name: bold_count_a
      struct:
      - name: '**'
        dtype: int64
      - name: __
        dtype: int64
    - name: context_a_tokens
      dtype: int32
    - name: sum_assistant_b_tokens
      dtype: int32
    - name: header_count_b
      struct:
      - name: h1
        dtype: int64
      - name: h2
        dtype: int64
      - name: h3
        dtype: int64
      - name: h4
        dtype: int64
      - name: h5
        dtype: int64
      - name: h6
        dtype: int64
    - name: list_count_b
      struct:
      - name: ordered
        dtype: int64
      - name: unordered
        dtype: int64
    - name: bold_count_b
      struct:
      - name: '**'
        dtype: int64
      - name: __
        dtype: int64
    - name: context_b_tokens
      dtype: int32
    - name: sum_user_tokens
      dtype: int32
    - name: turns
      dtype: int32
  - name: category_tag
    struct:
    - name: creative_writing_v0.1
      struct:
      - name: creative_writing
        dtype: bool
      - name: score
        dtype: string
    - name: criteria_v0.1
      struct:
      - name: complexity
        dtype: bool
      - name: creativity
        dtype: bool
      - name: domain_knowledge
        dtype: bool
      - name: problem_solving
        dtype: bool
      - name: real_world
        dtype: bool
      - name: specificity
        dtype: bool
      - name: technical_accuracy
        dtype: bool
    - name: if_v0.1
      struct:
      - name: if
        dtype: bool
      - name: score
        dtype: int32
    - name: math_v0.1
      struct:
      - name: math
        dtype: bool
  - name: language
    dtype: string
  - name: is_code
    dtype: bool
  - name: timestamp
    dtype: timestamp[ns]
  splits:
  - name: train
    num_bytes: 3029998372
    num_examples: 135634
  download_size: 1613803705
  dataset_size: 3029998372
configs:
- config_name: default
  data_files:
  - split: train
    path: data/train-*
license: cc-by-4.0
size_categories:
- 100K<n<1M
---
### Overview
This dataset contains user votes collected in the text-only category. Each row represents a single vote judging two models (model_a and model_b) on a user conversation, along with the full conversation history and metadata. Key fields include:
- `id`: Unique feedback ID of each vote/row.
- `evaluation_session_id`: Unique ID of each evaluation session, which can contain multiple separate votes/evaluations. 
- `evaluation_order`: Evaluation order of the current vote.
- `winner`: Battle result containing either model_a, model_b, tie, or both_bad.
- `conversation_a/conversation_b`: Full conversation of the current evaluation order.
- `full_conversation`: The entire conversation, including context prompts and answers from all previous evaluation orders. Note that after each vote new models are sampled, thus the responding models vary across the full context. 
- `conv_metadata`: Aggregated markdown and token counts for style control.
- `category_tag`: Annotation tags including the categories math, creative writing, hard prompts, and instruction following.
- `is_code`: Whether the conversation involves code.


### License

User prompts are licensed under CC-BY-4.0, and model outputs are governed by the terms of use set by the respective model providers.# chatbot_arena
