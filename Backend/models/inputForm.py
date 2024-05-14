from pydantic import BaseModel

class Summarizer(BaseModel):
    code:str

class GenerateCode(BaseModel):
    summary:str