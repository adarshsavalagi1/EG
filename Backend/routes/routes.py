from fastapi import APIRouter
from models.inputForm import Summarizer
from helpers.summariser import summarize


router=APIRouter()


@router.post("/summarize")
async def root(form:Summarizer):
    return {"summary": summarize(form.code)}
