! Class Declarations
! Generated file, do not Edit

doit
(Notification
	subclass: 'GsInteractionRequest'
	instVarNames: #(interaction)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanKernel
	options: #()
)
		category: 'GemStone-Interactions-Core';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'GsInteraction'
	instVarNames: #(defaultActionBlock)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanKernel
	options: #()
)
		category: 'GemStone-Interactions-Core';
		immediateInvariant.
true.
%

doit
(GsInteraction
	subclass: 'GsChoiceInteraction'
	instVarNames: #(prompt labels values lines)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanKernel
	options: #()
)
		category: 'GemStone-Interactions-Core';
		immediateInvariant.
true.
%

doit
(GsInteraction
	subclass: 'GsConfirmInteraction'
	instVarNames: #(prompt confirm cancel abort)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanKernel
	options: #()
)
		category: 'GemStone-Interactions-Core';
		immediateInvariant.
true.
%

doit
(GsConfirmInteraction
	subclass: 'GsNotifyInteraction'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanKernel
	options: #()
)
		category: 'GemStone-Interactions-Core';
		immediateInvariant.
true.
%

doit
(GsInteraction
	subclass: 'GsInformInteraction'
	instVarNames: #(message)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanKernel
	options: #()
)
		category: 'GemStone-Interactions-Core';
		immediateInvariant.
true.
%

doit
(GsInteraction
	subclass: 'GsInspectInteraction'
	instVarNames: #(theObject)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanKernel
	options: #()
)
		category: 'GemStone-Interactions-Core';
		immediateInvariant.
true.
%

doit
(GsInspectInteraction
	subclass: 'GsExploreInteraction'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanKernel
	options: #()
)
		category: 'GemStone-Interactions-Core';
		immediateInvariant.
true.
%

doit
(GsInteraction
	subclass: 'GsTextInteraction'
	instVarNames: #(prompt template requestPassword)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanKernel
	options: #()
)
		category: 'GemStone-Interactions-Core';
		immediateInvariant.
true.
%

doit
(GsTextInteraction
	subclass: 'GsMultiLineTextInteraction'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanKernel
	options: #()
)
		category: 'GemStone-Interactions-Core';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'GsInteractionHandler'
	instVarNames: #(choiceBlock confirmBlock informBlock textBlock multiLineTextBlock defaultBlock inspectBlock)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanKernel
	options: #()
)
		category: 'GemStone-Interactions-Core';
		immediateInvariant.
true.
%

! Class implementation for 'GsInteractionRequest'

!		Class methods for 'GsInteractionRequest'

category: 'interacting'
classmethod: GsInteractionRequest
signal: aGsInteraction
  ^ (self new interaction: aGsInteraction) signal
%

!		Instance methods for 'GsInteractionRequest'

category: 'signaling'
method: GsInteractionRequest
defaultAction
  ^ self interaction defaultActionFor: self
%

category: 'Compatibility'
method: GsInteractionRequest
gsArguments
  "This method included here for G/S 2.x only ... not needed nor used in 3.x"

  ^ {(self interaction)}
%

category: 'Instance initialization'
method: GsInteractionRequest
initialize
  super initialize.
  gsNumber := 121001
%

category: 'accessing'
method: GsInteractionRequest
interaction

   "Return the value of the instance variable 'interaction'."
   ^interaction
%

category: 'accessing'
method: GsInteractionRequest
interaction: anObject

   "Modify the value of the instance variable 'interaction'."
   interaction := anObject
%

category: 'signaling'
method: GsInteractionRequest
response: anObject
  self resume: anObject
%

! Class implementation for 'GsInteraction'

!		Instance methods for 'GsInteraction'

category: 'accessing'
method: GsInteraction
defaultActionBlock
  defaultActionBlock
    ifNil: [ 
      ^ [ :interactionRequest | 
      Transcript cr; show: self printString.
      nil ] ].
  ^ defaultActionBlock
%

category: 'accessing'
method: GsInteraction
defaultActionBlock: anObject

   "Modify the value of the instance variable 'defaultActionBlock'."
   defaultActionBlock := anObject
%

category: 'interacting'
method: GsInteraction
defaultActionFor: anInteractionRequest
  ^ self defaultActionBlock value: anInteractionRequest
%

category: 'interacting'
method: GsInteraction
interactWith: anObject
  "opportunity for double dispatch:

     interactWithChoice:
     interactWithConfirm:
     interactWithInform:
     interactWithInspect:
     interactWithMultiLineText:
     interactWithText:
  "

  self subclassResponsibility
%

category: 'printing'
method: GsInteraction
printLabel
  ^ ''
%

category: 'printing'
method: GsInteraction
printOn: aStream
  aStream nextPutAll: self class name asString , '(' , self printLabel , ')'
%

category: 'interacting'
method: GsInteraction
signal
  ^ GsInteractionRequest signal: self
%

! Class implementation for 'GsChoiceInteraction'

!		Class methods for 'GsChoiceInteraction'

category: 'instance creation'
classmethod: GsChoiceInteraction
labels: anArray
	^ self
		prompt: nil
		labels: anArray
		values: anArray
		lines: #()
%

category: 'instance creation'
classmethod: GsChoiceInteraction
labels: labelArray lines: lineArray
	^ self
		prompt: nil
		labels: labelArray
		values: labelArray
		lines: lineArray
%

category: 'instance creation'
classmethod: GsChoiceInteraction
prompt: aString labels: labelArray values: valueArray
	^ self
		prompt: aString
		labels: labelArray
		 values: valueArray
		lines: #()
%

category: 'instance creation'
classmethod: GsChoiceInteraction
prompt: aString labels: labelArray values: valueArray lines: lineArray
  ^ self new
    prompt: aString;
    labels: labelArray;
    values: valueArray;
    lines: lineArray;
    yourself
%

!		Instance methods for 'GsChoiceInteraction'

category: 'interacting'
method: GsChoiceInteraction
interactWith: anObject
  "opportunity for double dispatch:

     interactWithChoice:
     interactWithConfirm:
     interactWithInform:
     interactWithMultiLineText:
     interactWithText:
  "

  ^ anObject interactWithChoice: self
%

category: 'accessing'
method: GsChoiceInteraction
labels

   "Return the value of the instance variable 'labels'."
   ^labels
%

category: 'accessing'
method: GsChoiceInteraction
labels: anObject

   "Modify the value of the instance variable 'labels'."
   labels := anObject
%

category: 'accessing'
method: GsChoiceInteraction
lines

   "Return the value of the instance variable 'lines'."
   ^lines
%

category: 'accessing'
method: GsChoiceInteraction
lines: anObject

   "Modify the value of the instance variable 'lines'."
   lines := anObject
%

category: 'printing'
method: GsChoiceInteraction
printLabel
  ^ self prompt
%

category: 'accessing'
method: GsChoiceInteraction
prompt

   "Return the value of the instance variable 'prompt'."
   ^prompt
%

category: 'accessing'
method: GsChoiceInteraction
prompt: aString
  prompt := aString copyWrappedTo: 80
%

category: 'choice'
method: GsChoiceInteraction
select: anIndex
  ^ self values at: anIndex
%

category: 'choice'
method: GsChoiceInteraction
select: anIndex for: anInteractionRequest
  anInteractionRequest response: (self values at: anIndex)
%

category: 'accessing'
method: GsChoiceInteraction
values

   "Return the value of the instance variable 'values'."
   ^values
%

category: 'accessing'
method: GsChoiceInteraction
values: anObject

   "Modify the value of the instance variable 'values'."
   values := anObject
%

! Class implementation for 'GsConfirmInteraction'

!		Class methods for 'GsConfirmInteraction'

category: 'instance creation'
classmethod: GsConfirmInteraction
prompt: prompt
  ^ self prompt: prompt confirm: 'Ok'
%

category: 'instance creation'
classmethod: GsConfirmInteraction
prompt: prompt confirm: confirm
  ^ self prompt: prompt confirm: confirm cancel: 'Cancel'
%

category: 'instance creation'
classmethod: GsConfirmInteraction
prompt: prompt confirm: confirm cancel: cancel
  ^ self new
    prompt: prompt;
    confirm: confirm;
    cancel: cancel
%

category: 'instance creation'
classmethod: GsConfirmInteraction
prompt: prompt confirm: confirm cancel: cancel abort: abort
  "on confirm return true, on cancel return false on abort return nil"

  ^ self new
    prompt: prompt;
    confirm: confirm;
    cancel: cancel;
    abort: abort
%

!		Instance methods for 'GsConfirmInteraction'

category: 'accessing'
method: GsConfirmInteraction
abort
  ^ abort
%

category: 'accessing'
method: GsConfirmInteraction
abort: anObject
  abort := anObject
%

category: 'confirm'
method: GsConfirmInteraction
abortFor: anInteractionRequest
  anInteractionRequest response: self abortResponse
%

category: 'confirm'
method: GsConfirmInteraction
abortResponse
  ^ nil
%

category: 'accessing'
method: GsConfirmInteraction
cancel
  ^ cancel
%

category: 'accessing'
method: GsConfirmInteraction
cancel: anObject
	cancel := anObject
%

category: 'confirm'
method: GsConfirmInteraction
cancelFor: anInteractionRequest
  anInteractionRequest response: self cancelResponse
%

category: 'confirm'
method: GsConfirmInteraction
cancelResponse
  ^ false
%

category: 'accessing'
method: GsConfirmInteraction
confirm
	^ confirm
%

category: 'accessing'
method: GsConfirmInteraction
confirm: anObject
	confirm := anObject
%

category: 'accessing'
method: GsConfirmInteraction
defaultActionBlock
  defaultActionBlock
    ifNil: [ 
      ^ [ :interactionRequest | 
      Transcript cr; show: self printString.
      self cancelResponse ] ].
  ^ defaultActionBlock
%

category: 'interacting'
method: GsConfirmInteraction
interactWith: anObject
  "opportunity for double dispatch:

     interactWithChoice:
     interactWithConfirm:
     interactWithInform:
     interactWithMultiLineText:
     interactWithText:
  "

  ^ anObject interactWithConfirm: self
%

category: 'confirm'
method: GsConfirmInteraction
ok
  ^ true
%

category: 'confirm'
method: GsConfirmInteraction
okFor: anInteractionRequest
  anInteractionRequest response: self okResponse
%

category: 'confirm'
method: GsConfirmInteraction
okResponse
  ^ true
%

category: 'printing'
method: GsConfirmInteraction
printLabel
  ^ self prompt
%

category: 'accessing'
method: GsConfirmInteraction
prompt
	^ prompt
%

category: 'accessing'
method: GsConfirmInteraction
prompt: aString
  prompt := aString copyWrappedTo: 80
%

! Class implementation for 'GsNotifyInteraction'

!		Instance methods for 'GsNotifyInteraction'

category: 'accessing'
method: GsNotifyInteraction
defaultActionBlock
  "notify answers ok by default, while confirm answers fals by default"

  defaultActionBlock
    ifNil: [ 
      ^ [ :interactionRequest | 
      Transcript cr; show: self printString.
      self okResponse ] ].
  ^ defaultActionBlock
%

! Class implementation for 'GsInformInteraction'

!		Class methods for 'GsInformInteraction'

category: 'instance creation'
classmethod: GsInformInteraction
message: aString
  ^ self new
    message: aString;
    yourself
%

!		Instance methods for 'GsInformInteraction'

category: 'interacting'
method: GsInformInteraction
interactWith: anObject
  "opportunity for double dispatch:

     interactWithChoice:
     interactWithConfirm:
     interactWithInform:
     interactWithMultiLineText:
     interactWithText:
  "

  ^ anObject interactWithInform: self
%

category: 'accessing'
method: GsInformInteraction
message

   "Return the value of the instance variable 'message'."
   ^message
%

category: 'accessing'
method: GsInformInteraction
message: aString
  message := aString copyWrappedTo: 80
%

category: 'printing'
method: GsInformInteraction
printLabel
  ^ self message
%

! Class implementation for 'GsInspectInteraction'

!		Class methods for 'GsInspectInteraction'

category: 'instance creation'
classmethod: GsInspectInteraction
theObject: anObject
  ^ self new
    theObject: anObject;
    yourself
%

!		Instance methods for 'GsInspectInteraction'

category: 'accessing'
method: GsInspectInteraction
defaultActionBlock
  defaultActionBlock
    ifNil: [ 
      ^ [ :interactionRequest | 
      Transcript cr; show: self printString.
      self theObject ] ].
  ^ defaultActionBlock
%

category: 'interacting'
method: GsInspectInteraction
interactWith: anObject
  "opportunity for double dispatch:

     interactWithChoice:
     interactWithConfirm:
     interactWithInform:
     interactWithInspect:
     interactWithMultiLineText:
     interactWithText:
  "

  ^ anObject interactWithInspect: self
%

category: 'printing'
method: GsInspectInteraction
printLabel
  ^ self theObject printString
%

category: 'accessing'
method: GsInspectInteraction
theObject

   "Return the value of the instance variable 'theObject'."
   ^theObject
%

category: 'accessing'
method: GsInspectInteraction
theObject: anObject

   "Modify the value of the instance variable 'theObject'."
   theObject := anObject
%

! Class implementation for 'GsExploreInteraction'

!		Instance methods for 'GsExploreInteraction'

category: 'interacting'
method: GsExploreInteraction
interactWith: anObject
  "opportunity for double dispatch:

     interactWithChoice:
     interactWithConfirm:
     interactWithInform:
     interactWithInspect:
     interactWithMultiLineText:
     interactWithText:
  "

  ^ anObject interactWithExplore: self
%

! Class implementation for 'GsTextInteraction'

!		Class methods for 'GsTextInteraction'

category: 'instance creation'
classmethod: GsTextInteraction
prompt: aString
  ^ self prompt: aString template: ''
%

category: 'instance creation'
classmethod: GsTextInteraction
prompt: promptString template: templateString
  ^ self new
    prompt: promptString;
    template: templateString;
    yourself
%

category: 'instance creation'
classmethod: GsTextInteraction
requestPassword: aString
  ^ self new
    requestPassword: aString;
    yourself
%

!		Instance methods for 'GsTextInteraction'

category: 'accessing'
method: GsTextInteraction
defaultActionBlock
  defaultActionBlock
    ifNil: [ 
      ^ [ :interactionRequest | 
      Transcript cr; show: self printString.
      '' ] ].
  ^ defaultActionBlock
%

category: 'interacting'
method: GsTextInteraction
interactWith: anObject
  "opportunity for double dispatch:

     interactWithChoice:
     interactWithConfirm:
     interactWithInform:
     interactWithMultiLineText:
     interactWithText:
  "

  ^ anObject interactWithText: self
%

category: 'printing'
method: GsTextInteraction
printLabel
  ^ self prompt
%

category: 'accessing'
method: GsTextInteraction
prompt

   "Return the value of the instance variable 'prompt'."
   ^prompt
%

category: 'accessing'
method: GsTextInteraction
prompt: aString
  prompt := aString copyWrappedTo: 80
%

category: 'accessing'
method: GsTextInteraction
requestPassword

   requestPassword ifNil: [ requestPassword := false ].
   ^requestPassword
%

category: 'accessing'
method: GsTextInteraction
requestPassword: aString

   prompt := aString.
   requestPassword := true
%

category: 'accessing'
method: GsTextInteraction
template
  template ifNil: [ template := '' ].
  ^ template
%

category: 'accessing'
method: GsTextInteraction
template: anObject

   "Modify the value of the instance variable 'template'."
   template := anObject
%

! Class implementation for 'GsMultiLineTextInteraction'

!		Instance methods for 'GsMultiLineTextInteraction'

category: 'interacting'
method: GsMultiLineTextInteraction
interactWith: anObject
  "opportunity for double dispatch:

     interactWithChoice:
     interactWithConfirm:
     interactWithInform:
     interactWithMultiLineText:
     interactWithText:
  "

  ^ anObject interactWithMultiLineText: self
%

! Class implementation for 'GsInteractionHandler'

!		Instance methods for 'GsInteractionHandler'

category: 'accessing'
method: GsInteractionHandler
choiceBlock
  choiceBlock ifNil: [ ^ self defaultBlock ].
  ^ choiceBlock
%

category: 'accessing'
method: GsInteractionHandler
choiceBlock: anObject

   "Modify the value of the instance variable 'choiceBlock'."
   choiceBlock := anObject
%

category: 'accessing'
method: GsInteractionHandler
confirmBlock
  confirmBlock ifNil: [ ^ self defaultBlock ].
  ^ confirmBlock
%

category: 'accessing'
method: GsInteractionHandler
confirmBlock: anObject

   "Modify the value of the instance variable 'confirmBlock'."
   confirmBlock := anObject
%

category: 'accessing'
method: GsInteractionHandler
defaultBlock
  defaultBlock
    ifNil: [ 
      defaultBlock := [ :interaction | 
      self
        error:
          'No handler defined for ' , interaction class name asString , ' interaction.' ] ].
  ^ defaultBlock
%

category: 'accessing'
method: GsInteractionHandler
defaultBlock: anObject

   "Modify the value of the instance variable 'defaultBlock'."
   defaultBlock := anObject
%

category: 'accessing'
method: GsInteractionHandler
informBlock
  informBlock ifNil: [ ^ self defaultBlock ].
  ^ informBlock
%

category: 'accessing'
method: GsInteractionHandler
informBlock: anObject

   "Modify the value of the instance variable 'informBlock'."
   informBlock := anObject
%

category: 'accessing'
method: GsInteractionHandler
inspectBlock
  inspectBlock ifNil: [ ^ self defaultBlock ].
  ^ inspectBlock
%

category: 'accessing'
method: GsInteractionHandler
inspectBlock: anObject

   "Modify the value of the instance variable 'inspectBlock'."
   inspectBlock := anObject
%

category: 'interactions'
method: GsInteractionHandler
interactWithChoice: interaction
  ^ self choiceBlock value: interaction
%

category: 'interactions'
method: GsInteractionHandler
interactWithConfirm: interaction
  ^ self confirmBlock value: interaction
%

category: 'interactions'
method: GsInteractionHandler
interactWithExplore: interaction
  ^ self interactWithInspect: interaction
%

category: 'interactions'
method: GsInteractionHandler
interactWithInform: interaction
  ^ self informBlock value: interaction
%

category: 'interactions'
method: GsInteractionHandler
interactWithInspect: interaction
  ^ self inspectBlock value: interaction
%

category: 'interactions'
method: GsInteractionHandler
interactWithMultiLineText: interaction
  ^ self multiLineTextBlock value: interaction
%

category: 'interactions'
method: GsInteractionHandler
interactWithText: interaction
  ^ self textBlock value: interaction
%

category: 'accessing'
method: GsInteractionHandler
multiLineTextBlock
  multiLineTextBlock ifNil: [ ^ self defaultBlock ].
  ^ multiLineTextBlock
%

category: 'accessing'
method: GsInteractionHandler
multiLineTextBlock: anObject

   "Modify the value of the instance variable 'multiLineTextBlock'."
   multiLineTextBlock := anObject
%

category: 'accessing'
method: GsInteractionHandler
textBlock
  textBlock ifNil: [ ^ self defaultBlock ].
  ^ textBlock
%

category: 'accessing'
method: GsInteractionHandler
textBlock: anObject

   "Modify the value of the instance variable 'textBlock'."
   textBlock := anObject
%

! Class extensions for 'Object'

!		Instance methods for 'Object'

category: '*gemstone-interactions-kernel'
method: Object
confirm: aString
  ^ (GsConfirmInteraction prompt: aString) signal
%

category: '*gemstone-interactions-kernel'
method: Object
inform: aString
  "Display aString to the user."

  ^ (GsInformInteraction message: aString) signal
%

category: '*gemstone-interactions-kernel'
method: Object
inspect
  ^ (GsInspectInteraction theObject: self) signal
%

! Class extensions for 'Warning'

!		Instance methods for 'Warning'

category: '*gemstone-interactions-kernel'
method: Warning
defaultAction
  "The user should be notified of the occurrence of an exceptional
	occurrence and given an option of continuing or aborting the
	computation. The description of the occurrence should include
	any text specified as the argument of the #signal: message."

  | response |
  response := (GsNotifyInteraction
    prompt:
      self description withoutGemstoneLineEndings , ' Press ''Proceed'' to continue.'
    confirm: 'Proceed'
    cancel: 'Debug'
    abort: 'Cancel') signal.
  response == true
    ifTrue: [ ^ super defaultAction ].
  response == false
    ifTrue: [ self halt: 'Debugging: ' , self description ].
  ^ Processor activeProcess terminate
%

