// Applicative|ApplicativePlus|ApplyBind|CommutativeRing|Con|EuclideanRing|EuclideanRingField|Functor|FunctorAlt|FunctorExtend|RingCommutativeRing

var dot = `
  digraph {
    graph [label="" labelloc="t", fontsize="20.0"]
    node [style="filled" fillcolor="#d0d0d0"]
    
    Semigroup [id="Semigroup" label="Semigroup" class="clickable"]
    Monoid [id="Monoid" label="Monoid" class="clickable" shape="box"]
    Semigroup -> Monoid [id="SemigroupMonoid" class="clickable"]
    
    Semiring [id="Semiring" label="Semiring" class="clickable"]
    Ring [id="Ring" label="Ring" class="clickable"]
    CommutativeRing [id="CommutativeRing" label="CommutativeRing" style="dashed"]
    EuclideanRing [id="EuclideanRing" label="EuclideanRing" style="dashed"]
    DivisionRing [id="DivisionRing" label="DivisionRing" class="clickable"]
    Field [id="Field" label="Field" class="clickable" shape="box"]
    Semiring -> Ring [id="SemiringRing" class="clickable"]
    Ring -> CommutativeRing [id="RingCommutativeRing" class="clickable" style="dashed"]
    CommutativeRing -> DivisionRing [id="CommutativeRingDivisionRing" style="dashed" class="clickable" style="dashed"]
    CommutativeRing -> EuclideanRing [id="CommutativeRingEuclideanRing" style="dashed"]
    Ring -> DivisionRing [id="RingDivisionRing" class="clickable"]
    EuclideanRing -> Field [id="EuclideanRingField" style="dashed"]
    
    Con [id="Con" label="* -> *" style="dashed"]
    Functor [id="Functor" label="Functor" style="dashed"]
    Apply [id="Apply" label="Apply" class="clickable"]
    Applicative [id="Applicative" label="Applicative" style="dashed"]
    Bind [id="Bind" label="Bind" class="clickable"]
    Monad [id="Monad" label="Monad" class="clickable" shape="box"]
    Con -> Functor [id="ConFunctor" style="dashed" class="clickable" style="dashed"]
    Functor -> Apply [id="FunctorApply" style="dashed"]
    Apply -> Applicative [id="ApplyApplicative" class="clickable"]
    Apply -> Bind [id="ApplyBind" class="clickable" style="dashed"]
    Applicative -> Monad [id="ApplicativeMonad" style="dashed"]
    Bind -> Monad [id="BindMonad" class="clickable"]
    
    Extend [id="Extend" label="Extend" class="clickable"]
    Comonad [id="Comonad" label="Comonad" class="clickable" shape="box"]
    Functor -> Extend [id="FunctorExtend" style="dashed"]
    Extend -> Comonad [id="ExtendComonad" class="clickable"]
    
    Alt [id="Alt" label="Alt" class="clickable"]
    Plus [id="Plus" label="Plus" class="clickable"]
    Alternative [id="Alternative" label="Alternative" class="clickable" shape="box"]
    Functor -> Alt [id="FunctorAlt" style="dashed"]
    Alt -> Plus [id="AltPlus" class="clickable"]
    Applicative -> Alternative [id="ApplicativePlus" style="dashed"]
    Plus -> Alternative [id="PlusAlternative" class="clickable"]
    
    Semigroupoid [id="Semigroupoid" label="Semigroupoid" class="clickable"]
    Category [id="Category" label="Category" class="clickable" shape="box"]
    Semigroupoid -> Category [id="SemigroupoidCategory" class="clickable"]
    
    Profunctor [id="Profunctor" label="Profunctor" class="clickable"]
    Strong [id="Strong" label="Strong" class="clickable"]
    Arrow [id="Arrow" label="Arrow" class="clickable" shape="box"]
    Profunctor -> Strong [id="ProfunctorStrong" class="clickable"]
    Category -> Arrow [id="CategoryArrow" class="clickable"]
    Strong -> Arrow [id="StrongArrow" class="clickable"]
  }`;

var graphviz = d3.select("#graph").graphviz({ 
  zoomScaleExtent: [1, 1]
});
    
graphviz.renderDot(dot).on("end", function () {
  
  d3.selectAll('.clickable')
    .on("mouseover", function() {
      
      d3.select(this).attr('stroke-width', function() { 
        return 1.5
      });
    })
    .on("mouseout", function() {
      
      d3.select(this).attr('stroke-width', function() {
        return 1.0
      });
    })
    .on("click", function() {
      const sectionId = this.id;
      
       $('.section:not(#section_' + sectionId + ')').css("display", "none");
       $('#section_' + sectionId).show(0, function() {
         this.scrollIntoView({ behavior: 'smooth' });
         
       });
         
    });
});