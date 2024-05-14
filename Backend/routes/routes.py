from fastapi import APIRouter
from models.inputForm import Summarizer,GenerateCode
from helpers.summariser import summarize,generate_code
router=APIRouter()


@router.post("/summarize")
async def root(form:Summarizer):
    return {"summary": summarize(form.code)}

@router.post("/generate-code")
async def generate_coder(form:GenerateCode):
    return {"code":generate_code(form.summary)}