# Development Architecture Planning

1. This document has been merged into `PLANNING.md`, which now consolidates the architecture roadmap and the transformation workstream.

## Migration Strategy

1. All new functions have "_dev" suffix to avoid conflicts
2. Existing functions remain unchanged
3. Documentation clearly indicates which functions to use
4. Gradual migration as users adopt new workflow
5. Eventually deprecate old functions

## Testing Plan

1. Unit tests for each new function
2. Integration tests for complete workflows
3. Performance benchmarks vs old functions
4. Missing data scenario testing
5. Edge case validation

## Next Steps

Continue implementing high-priority components:
- margot_hetero_dev()
- margot_policy_tree_dev()
- margot_rate_dev()
- margot_flip_forests_dev()

Focus on maintaining consistency with completed components and ensuring smooth integration across the ecosystem.
