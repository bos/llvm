#ifdef declare_or_define_pass

/* LLVM-C passes */

declare_or_define_pass( AAEval )
declare_or_define_pass( AliasAnalysisCounter )
#if HS_LLVM_VERSION < 302
declare_or_define_pass( AlwaysInliner )
#endif
// Name conflicts with those in LLVM proper, have a safer prefix?
// declare_or_define_pass( BasicAliasAnalysis )
declare_or_define_pass( BlockPlacement )
declare_or_define_pass( BreakCriticalEdges )
declare_or_define_pass( CodeGenPrepare )
#if HS_LLVM_VERSION < 303
declare_or_define_pass( DbgInfoPrinter )
#endif
declare_or_define_pass( DeadCodeElimination )
declare_or_define_pass( DeadInstElimination )
declare_or_define_pass( DemoteRegisterToMemory )
declare_or_define_pass( DomOnlyPrinter )
declare_or_define_pass( DomOnlyViewer )
declare_or_define_pass( DomPrinter )
declare_or_define_pass( DomViewer )
declare_or_define_pass( EdgeProfiler )
declare_or_define_pass( GlobalsModRef )
declare_or_define_pass( InstCount )
declare_or_define_pass( InstructionNamer )
declare_or_define_pass( LazyValueInfo )
declare_or_define_pass( LCSSA )
#if HS_LLVM_VERSION < 302
declare_or_define_pass( LoopDependenceAnalysis )
#endif
declare_or_define_pass( LoopExtractor )
declare_or_define_pass( LoopSimplify )
declare_or_define_pass( LoopStrengthReduce )
declare_or_define_pass( LowerInvoke )
declare_or_define_pass( LowerSwitch )
declare_or_define_pass( MergeFunctions )
declare_or_define_pass( NoAA )
declare_or_define_pass( NoProfileInfo )
declare_or_define_pass( OptimalEdgeProfiler )
declare_or_define_pass( PartialInlining )
declare_or_define_pass( PostDomOnlyPrinter )
declare_or_define_pass( PostDomOnlyViewer )
declare_or_define_pass( PostDomPrinter )
declare_or_define_pass( PostDomViewer )
declare_or_define_pass( ProfileEstimator )
declare_or_define_pass( ProfileLoader )
declare_or_define_pass( ProfileVerifier )
declare_or_define_pass( ScalarEvolutionAliasAnalysis )
declare_or_define_pass( SingleLoopExtractor )
declare_or_define_pass( StripNonDebugSymbols )
#if HS_LLVM_VERSION < 300
declare_or_define_pass( StructRetPromotion )
declare_or_define_pass( TailDuplication )
#endif
declare_or_define_pass( UnifyFunctionExitNodes )



/* Passes declared in extra.cpp goes here */

declare_or_define_pass( Internalize2 )

#endif
