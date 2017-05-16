# The вotnik (type Votnik f st r = forall f st r. Plot... ) automator

For creating interactive automations with multiple well abstracted backends.

The idea is simple, you define automations in a DSL. That DSL has a standard library including Haskell statistical libraries and more. Plotting is a first class citien, and the UI is HTML and AST (lucky you, a Hskell DSL!).

If one UI provider dies, you have alternative implementations of the DSL, e.g.
web-forms vs hipchat.
